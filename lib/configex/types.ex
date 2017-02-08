defmodule Configex.Types do
  @moduledoc false

  require Configex.Gettext

  @builtin_types [:string, :integer, :float, :map, :list]

  def default_value(:list), do: []
  def default_value({:list, _}), do: []
  def default_value(:map), do: %{}
  def default_value({:map, _}), do: %{}
  def default_value(_), do: nil

  def validate_type!(type) when type in @builtin_types, do: :ok
  def validate_type!({:list, sub_type}), do: validate_type!(sub_type)
  def validate_type!({:map, sub_type}), do: validate_type!(sub_type)
  def validate_type!(type) when is_atom(type) do
    case Atom.to_charlist(type) do
      'Elixir' ++ _ ->
        unless function_exported?(type, :__struct__, 0) do
          raise ArgumentError, "#{inspect type} doesn't define a struct"
        end
        :ok
      _ ->
        raise ArgumentError, "Unsupported primitive type #{type}. Could only be #{inspect @builtin_types}"
    end
  end

  def validate_type!(type) do
    raise ArgumentError, "Unsupported type #{type}. Could only be #{inspect @builtin_types} or struct module"
  end

  defmacro validate_error(path, expect_type, value) do
    quote do
      {:error, Configex.Gettext.dgettext("error", "%{path} should be a %{type} but got %{value}", %{type: unquote(expect_type), path: unquote(path), value: unquote(value)})}
    end
  end

  def validate_value(:string, _path, value) when is_binary(value), do: :ok
  def validate_value(:string, path, value), do: validate_error(path, "string", value)

  def validate_value(:integer, _path, value) when is_integer(value), do: :ok
  def validate_value(:integer, path, value), do: validate_error(path, "integer", value)

  def validate_value(:float, _path, value) when is_integer(value) or is_float(value), do: :ok
  def validate_value(:float, path, value), do: validate_error(path, "float", value)

  def validate_value(:map, _path, value) when is_map(value), do: :ok
  def validate_value(:map, path, value), do: validate_error(path, "map", value)

  def validate_value(:list, _path, value) when is_list(value), do: :ok
  def validate_value(:list, path, value), do: validate_error(path, "list", value)

  def validate_value({:list, type}, path, value) when is_list(value) do
    value
    |> Enum.with_index
    |> Enum.reduce_while(:ok, fn
      {value, index}, :ok ->
        {:cont, validate_value(type, path <> "[#{index}]", value)}
      _, {:error, _} = error -> {:halt, error}
    end)
  end
  def validate_value({:list, _type}, path, value), do: validate_error(path, "list", value)

  def validate_value({:map, type}, path, value) when is_map(value) do
    value
    |> Enum.reduce_while(:ok, fn
      {key, value}, :ok ->
        {:cont, validate_value(type, path <> "[#{key}]", value)}
      _, {:error, _} = error -> {:halt, error}
    end)
  end
  def validate_value({:map, _type}, path, value), do: validate_error(path, "map", value)

  def validate_value(_module, _path, _value), do: :ok

  defmacro cast_error(expect_type, value) do
    quote do
      string =
      {:error, Configex.Gettext.dgettext("error", "expect a %{type} but got %{value}", %{type: unquote(expect_type) |> inspect, value: unquote(value) |> inspect})}
    end
  end

  def cast(:integer, integer) when is_integer(integer), do: {:ok, integer}
  def cast(:integer, binary) when is_binary(binary) do
    case Integer.parse(binary) do
      {int, _} -> {:ok, int}
      :error -> cast_error(:integer, binary)
    end
  end
  def cast(:integer, other), do: cast_error(:integer, other)

  def cast(:string, binary) when is_binary(binary), do: {:ok, binary}
  def cast(:string, other), do: {:ok, inspect(other)}

  def cast(:float, float) when is_float(float) or is_integer(float), do: {:ok, float + 0.0}
  def cast(:float, binary) when is_binary(binary) do
    case Float.parse(binary) do
      {float, _} -> {:ok, float}
      :error -> cast_error(:float, binary)
    end
  end

  def cast(:float, other), do: cast_error(:float, other)

  def cast(:list, list) when is_list(list), do: {:ok, list}
  def cast(:list, binary) when is_binary(binary) do
    list = String.split(binary, ",", trim: true) |> Enum.map(&String.trim/1)
    {:ok, list}
  end
  def cast(:list, value) when is_integer(value) or is_float(value), do: {:ok, [value]}
  def cast(:list, other), do: cast_error(:list, other)

  def cast(:map, map) when is_map(map), do: {:ok, map}
  def cast(:map, kw) when is_list(kw) do
    Enum.reduce_while(kw, {:ok, Map.new}, fn
      {k, v}, {:ok, map} ->
        {:cont, {:ok, Map.put(map, to_string(k), v)}}
      _, _ ->
        {:halt, cast_error(:map, kw)}
    end)
  end
  def cast(:map, other), do: cast_error(:map, other)

  def cast({:list, other_type}, list) do
    with {:ok, list} <- cast(:list, list),
         {:ok, list} <- do_cast_children(other_type, list) do
      {:ok, list |> Enum.reverse}
    else
      _ -> cast_error({:list, other_type}, list)
    end
  end


  def cast({:map, other_type}, map) do
    with {:ok, map} <- cast(:map, map),
         {:ok, map} <- do_cast_children(other_type, map) do
      {:ok, map}
    else
      _ -> cast_error({:map, other_type}, map)
    end
  end

  def cast(module, values) when is_atom(module) and is_map(values) do
    with {:ok, values} <- cast_values(values),
         {:ok, struct} <- do_cast_struct(module, values) do
      {:ok, struct}
    else
      _ -> cast_error(module, values)
    end
  end

  def cast(struct, values), do: cast_error(struct, values)

  def do_cast_children(type, list) when is_list(list)do
    Enum.reduce_while(list, {:ok, []}, fn
      item, {:ok, list} ->
        case cast(type, item) do
          {:ok, item} -> {:cont, {:ok, [item | list]}}
          {:error, _} = error -> {:halt, error}
        end
    end)
  end

  def do_cast_children(type, map) when is_map(map)do
    Enum.reduce_while(map, {:ok, Map.new}, fn
      {key, value}, {:ok, map} ->
        case cast(type, value) do
          {:ok, item} -> {:cont, {:ok, Map.put(map, to_string(key), item)}}
          {:error, _} = error -> {:halt, error}
        end
    end)
  end

  defp do_cast_struct(struct, values) do
    try do
      {:ok, struct(struct, values)}
    rescue
      ArgumentError ->
        {:error, :error}
    end
  end

  defp cast_values(values) do
    Enum.reduce_while(values, {:ok, Map.new}, fn
      {key, value}, {:ok, map} when is_binary(key) ->
        case do_cast_binary_key(key) do
          {:ok, atom} -> {:cont, {:ok, Map.put(map, atom, value)}}
          {:error, _} = error -> {:halt, error}
        end
      {key, value}, {:ok, map} when is_atom(key) ->
        {:cont, {:ok, Map.put(map, key, value)}}
    end)
  end

  defp do_cast_binary_key(key) do
    try do
      {:ok, String.to_existing_atom(key)}
    rescue
      ArgumentError ->
        {:error, :error}
    end
  end
end