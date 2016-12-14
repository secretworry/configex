defmodule Configex.Builder do

  @configs :__configex_configs__
  @otp_app :__configex_otp_app__
  @adapter :__configex_adapter__
  @config  :__configex_config__

  defmacro __using__(opts) do
    quote do
      IO.puts "import #{inspect unquote(__MODULE__), only: [config: 2, config: 3]}"
      import unquote(__MODULE__), only: [config: 2, config: 3]
      Module.register_attribute __MODULE__, unquote(@configs), accumulate: false, persist: false
      {otp_app, adapter, config} = Configex.Builder.parse_config(__MODULE__, unquote(opts))
      Module.put_attribute __MODULE__, unquote(@otp_app), otp_app
      Module.put_attribute __MODULE__, unquote(@adapter), adapter
      Module.put_attribute __MODULE__, unquote(@config), config

      @before_compile unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(env) do
    [
      define_accessors(env),
      define_callbacks
    ]
  end

  defp define_accessors(env) do
    config = Module.get_attribute(env.module, :config)
    adapter = Module.get_attribute(env.module, :adapter)
    configs = Module.get_attribute(env.module, @configs) || []
    quoted_config_accessors = Enum.reduce(configs, nil, &quote_config_accessor/2)
    quote do
      def __configex__(:adapter), do: unquote(adapter)
      def __configex__(:config), do: unquote(config)
      unquote(quoted_config_accessors)
      def __configex__(:config, config) do
        raise ArgumentError, "Cannot find config #{inspect config}"
      end
      def __configex__(:configs), do: unquote(configs |> Macro.escape)
    end
  end

  defp quote_config_accessor(config, acc) do
    quote do
      unquote(acc)
      def __callback__(:config, unquote(config.name)), do: unquote(config |> Macro.escape)
    end
  end

  defp define_gen_servers() do
    quote do
      use GenServer

      def start_link do
        GenServer.start_link(__MODULE__, [], name: __MODULE__)
      end

      def start do
        GenServer.start(__MODULE__, [], name: __MODULE__)
      end

      def init(_) do
        case reload do
          {:ok, values} ->
            Process.flag(:trap_exit, true)
            {:ok, {:loaded, values}}
          {:error, error} -> {:stop, error}
        end
      end

      defp reload do
        __configex__(:configs) |> Enum.reduce_while({:ok, %{}}, fn
          config, {:ok, map} ->
            case do_get(config, false) do
              {:ok, value} -> {:cont, {:ok, Map.put(map, config.name, value)}}
              error -> {:halt, error}
            end
        end)
      end

      def handle_call({:get, config}, _from, {state, map}) do
        {:reply, Map.get(map, config.name), {state, map}}
      end

      def hand_call({:put, config, value}, _from, {state, map}) do
        with :ok <- do_put(config, value) do
          {:reply, :ok, {state, Map.put(map, config.name, value)}}
        end
      end

      def handle_cast(:changed, {:loaded, map}) do
        parent = self
        pid = spawn_link(fn->
          case reload do
            {:ok, values} -> parent ! {:loaded, values}
            error -> :failed
          end
        end)
        {:noreply, {{:loading, pid}, map}}
      end


      defp do_get(config, notify) do
        {adapter, opts} = __configex__(:adapter)
        result = if notify do
          adapter.get(config.name, self, opts)
        else
          adapter.get(config.name, opts)
        end
        case result do
          {:ok, nil} -> {:ok, config.default}
          {:ok, _} = ok -> ok
          error -> error
        end
      end

      defp do_put(config, value) do
        with :ok <- config.validator.(value) do
          {adapter, opts} = __configex__(:adapter)
          adapter.put(config.name, value, opts)
        end
      end

      def handle_info({:loaded, values}, {{:loading, _}, _map}) do
        {:noreply, {:loaded, values}}
      end

      def hanle_info({'EXIT', from, reason}, {{:loading, from}, map}) do
        {:noreply, {:loaded, map}}
      end

      def handle_info(info, state), do: super(info, state)
    end
  end

  defp define_callbacks do
    quote do
      require Configex.Gettext

      def get(name) do
        config = __configex__(:config, name)
        GenServer.call(__MODULE__, {:get, config})
      end

      def put(name, value) do
        config = __configex__(:config, name)
        GenServer.call(__MODULE__, {:put, config, value})
      end

      def cast(name, value) do
        config = __configex__(:config, name)
        with {:ok, value} <- Configex.Types.cast(config, config.type, value),
             :ok          <- put(name, value),
         do: {:ok, value}
      end

      def changed(name) do
        config = __configex__(:config, name)
        GenServer.cast(__MODULE__, {:changed, config})
      end

      def changed do
        GenServer.cast(__MODULE__, :changed)
      end

      def get!(name) do
        case get(name) do
          {:ok, value} -> value
          {:error, error} -> raise Configex.ConfigError, inspect(error)
        end
      end

      def put!(name, value) do
        case put(name, value) do
          :ok -> :ok
          {:error, error} -> raise Configex.ConfigError, inspect(error)
        end
      end

      def cast!(name, value) do
        case cast(name, value) do
          {:ok, value} -> value
          {:error, error} -> raise Configex.ConfigError, inspect(error)
        end
      end

    end
  end

  defmacro config(name, type, opts \\ []) do
    module = __CALLER__.module
    configs = Module.get_attribute(module, @configs) || []
    validate_name!(name, configs)
    Configex.Types.validate_type!(type)
    {config, code} = build_config(__CALLER__, name, type, opts)
    Module.put_attribute(module, @configs, [config | configs])
    quote do
      unquote(code)
    end
  end

  defp build_config(env, name, type, opts) do
    default_value = Keyword.get(opts, :default)
    {validator, code} = Keyword.get(opts, :validator, nil)
                |> init_validator(env.module, name)
    {%Configex.Config{module: env.module, name: name, type: type, default: default_value, validator: validator, line: env.line, file: env.file}, code}
  end

  def init_validator(nil, _module, _name) do
    {nil, nil}
  end

  def init_validator(method, module, _name) when is_atom(method) do
    unless Module.defines?(module, {method, 1}) do
      raise ArgumentError, "#{inspect module} should define validator #{method}/1 in #{inspect module}"
    end
    {method, nil}
  end

  def init_validator({method, opts}, module, _name) when is_atom(method) do
    unless Module.defines?(module, {method, 2}) do
      raise ArgumentError, "#{inspect module} should define validator #{method}/2 in #{inspect module}"
    end
    {{method, opts}, nil}
  end

  def init_validator(func, module, name) when is_function(func, 1) do
    method_name = "__configex_validator_#{name}__" |> String.to_atom
    {method_name, quote do
      def unquote(method_name)(value) do
        unquote(func).(value)
      end
    end}
  end

  def init_validator(func, _module, name) do
    raise ArgumentError, "validator should be atom, {atom, any} or function/1 but got #{inspect func} for #{name}"
  end

  defp validate_name!(name, configs) do
    if (conflict_definition = Enum.find(configs, &(&1.name == name))) do
      raise ArgumentError, "#{inspect name} has been defined at #{conflict_definition.file}:#{conflict_definition.line}"
    end
  end

  def parse_config(module, opts) do
    otp_app = Keyword.fetch!(opts, :otp_app)
    config = Application.get_env(otp_app, module, [])
    adapter = opts[:adapter] || config[:adapter]

    unless adapter do
      raise ArgumentError, "missing :adapter configuration in config #{inspect otp_app}, #{inspect module}"
    end
    {otp_app, init_adapter(adapter), config |> Enum.into(%{})}
  end

  defp init_adapter({adapter, opts}) do
    {adapter, adapter.init(opts)}
  end

  defp init_adapter(adapter) when is_atom(adapter) do
    {adapter, adapter.init([])}
  end
end