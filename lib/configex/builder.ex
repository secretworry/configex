defmodule Configex.Builder do


  @configs :__configex_configs__
  @otp_app :__configex_otp_app__
  @adapter :__configex_adapter__
  @config  :__configex_config__

  defmacro __using__(opts) do
    quote do
      import unquote(__MODULE__), only: [config: 2, config: 3]
      require Configex.Gettext
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
      define_callbacks,
      define_gen_servers
    ]
  end

  defp define_accessors(env) do
    config = Module.get_attribute(env.module, @config)
    adapter = Module.get_attribute(env.module, @adapter)
    configs = Module.get_attribute(env.module, @configs) || []
    quoted_config_accessors = Enum.map(configs, &quote_config_accessor/1)
    quote do
      def __configex__(:adapter), do: unquote(adapter)
      def __configex__(:config), do: unquote(config |> Macro.escape)
      unquote(quoted_config_accessors)
      def __configex__(:config, config) do
        nil
      end
      def __configex__(:configs), do: unquote(configs |> Macro.escape)
    end
  end

  defp quote_config_accessor(config) do
    quote do
      def __configex__(:config, unquote(config.name)), do: unquote(config |> Macro.escape)
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
        with {:ok, _}      <- do_ensure_adapter_started(),
             {:ok, values} <- do_reload do
          Process.flag(:trap_exit, true)
          {:ok, {:loaded, values}}
        else
          {:error, error} -> {:stop, error}
          error -> {:stop, error}
        end
      end

      defp do_ensure_adapter_started() do
        {adapter, opts} = __configex__(:adapter)
        # Config is important, so we assuming any abnormal error in the config server should tears down the node
        # TODO think about this later
        adapter.ensure_all_started(:transient, opts)
      end

      defp do_reload do
        __configex__(:configs) |> Enum.reduce_while({:ok, %{}}, fn
          config, {:ok, map} ->
            case do_get(config, false) do
              {:ok, value} -> {:cont, {:ok, Map.put(map, config.name, value)}}
              error -> {:halt, error}
            end
        end)
      end

      def handle_call({:get, config}, _from, {state, map}) do
        {:reply, {:ok, Map.get(map, config.name)}, {state, map}}
      end

      def handle_call({:put, config, value}, _from, {state, map}) do
        with :ok <- do_put(config, value) do
          {:reply, :ok, {state, Map.put(map, config.name, value)}}
        else
          error ->
            {:reply, error, {state, map}}
        end
      end

      def handle_call(:changed, from, {{:loading, pid, waiter}, map}) do
        {:noreply, {{:loading, pid, [from|waiter]}, map}}
      end

      def handle_call(:changed, from, {:loaded, map}) do
        pid = reload(self)
        {:noreply, {{:loading, pid, [from]}, map}}
      end

      def handle_cast(:changed, {{:loading, _, _} = state, map}) do
        {:noreply, {state, map}}
      end

      def handle_cast(:changed, {:loaded, map}) do
        pid = reload(self)
        {:noreply, {{:loading, pid, []}, map}}
      end

      defp reload(notify_to) do
        spawn_link(fn->
          case do_reload do
            {:ok, values} -> send(notify_to, {:loaded, values})
            error -> :failed
          end
        end)
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
          {:ok, value} -> Configex.Types.cast(config.type, value)
          {:error, _} = error -> error
          value -> raise "Expect adapter to return {:ok, value} of {:error, error} but got #{inspect value}"
        end
      end

      defp do_put(config, value) do
        with :ok <- Configex.Types.validate_value(config.type, to_string(config.name), value) do
          case Configex.Config.call_validator(config.validator, config.module, value) do
            result when result in [:ok, true] ->
              {adapter, opts} = __configex__(:adapter)
              adapter.put(config.name, value, opts)
            false ->
              {:error, Configex.Gettext.dgettext("error", "Invalid value %{value}", value: inspect(value))}
            {:error, _} = error -> error
            value -> raise "Expect validator #{inspect config.validator} to return :ok, {:error error}, true or false, but got #{inspect value}"
          end
        end
      end

      def handle_info({:loaded, values}, {{:loading, _, waiters}, _map}) do
        Enum.each(waiters, &(GenServer.reply(&1, :ok)))
        {:noreply, {:loaded, values}}
      end

      def hanle_info({'EXIT', from, reason}, {{:loading, from, waiters}, map}) do
        Enum.each(waiters, &(GenServer.reply(&1, {:error, reason})))
        {:noreply, {:loaded, map}}
      end

      def handle_info(info, state), do: super(info, state)
    end
  end

  defp define_callbacks do
    quote do
      require Configex.Gettext

      def get(name) do
        with {:ok, config} <- do_get_config(name) do
          GenServer.call(__MODULE__, {:get, config})
        end
      end

      def put(name, value) do
        with {:ok, config} <- do_get_config(name) do
          GenServer.call(__MODULE__, {:put, config, value})
        end
      end

      def cast(name, value) do
        with {:ok, config} <- do_get_config(name),
             {:ok, value}  <- Configex.Types.cast(config.type, value),
             :ok           <- put(name, value),
         do: {:ok, value}
      end

      defp do_get_config(name) do
        case __configex__(:config, name) do
          nil ->
            {:error, Configex.Gettext.dgettext("error", "Config %{name} does not exist", name: name)}
          config ->
            {:ok, config}
        end
      end


      def changed(name) do
        config = __configex__(:config, name)
        GenServer.cast(__MODULE__, {:changed, config})
      end

      def changed do
        GenServer.cast(__MODULE__, :changed)
      end

      def changed! do
        GenServer.call(__MODULE__, :changed)
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
    type = Macro.expand(type, __CALLER__)
    opts = Macro.expand(opts, __CALLER__)
    Configex.Types.validate_type!(type)
    {config, code} = build_config(__CALLER__, name, type, opts)
    quote do
      configs = Module.get_attribute(__MODULE__, unquote(@configs)) || []
      Configex.Builder.validate_name!(unquote(name), configs)
      config = %Configex.Config{unquote_splicing(config)}
      Module.put_attribute(__MODULE__, unquote(@configs), [config|configs])
      unquote(code)
    end
  end

  defp build_config(env, name, type, opts) do
    default_value = Keyword.get(opts, :default) || (Configex.Types.default_value(type) |> Macro.escape)
    {validator, code} = Keyword.get(opts, :validator, nil)
                |> init_validator(env.module, name)
    {[module: env.module, name: name, type: type, default: default_value, validator: validator, line: env.line, file: env.file], code}
  end

  def method_name(name) do
    "__configex_validator_#{name}__" |> String.to_atom
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

  def init_validator({:fn, _, [{:->, _, [[_], _]}|_]} = func, _module, name) do
    method_name = method_name(name)
    {method_name, quote_function_call(method_name, func)}
  end

  # init validator like &Module.foo/1
  def init_validator({:&, _, [{:/, _, [_, arity]}]} = func, _module, name) do
    if arity == 1 do
      method_name = method_name(name)
      {method_name, quote_function_call(method_name, func)}
    else
      invalid_validator(func, name)
    end
  end

  def init_validator({:&, _, body} = func, _module, name) do
    placeholder_count = count_placeholder(body)
    if placeholder_count == 1 do
      method_name = method_name(name)
      {method_name, quote_function_call(method_name, func)}
    else
      invalid_validator(func, name)
    end
  end

  def init_validator(func, _module, name) do
    invalid_validator(func, name)
  end

  def quote_function_call(method_name, func) do
    quote do
      def unquote(method_name)(value) do
        try do
          unquote(func).(value)
        rescue
          _ -> {:error, Configex.Gettext.dgettext("error", "Invalid value %{value}", value: inspect(value))}
        end
      end
    end
  end

  defp invalid_validator(func, name) do
    raise ArgumentError, "validator should be atom, {atom, any} or function/1 but got #{func |> Macro.to_string} for #{name}"
  end

  defp count_placeholder(body) do
    {_, map_set} = Macro.prewalk(body, MapSet.new, fn
      {:&, _, [id]} = node, map_set-> {node, MapSet.put(map_set, id)}
      node, map_set -> {node, map_set}
    end)
    MapSet.size(map_set)
  end

  def validate_name!(name, configs) do
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