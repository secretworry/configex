defmodule Configex do
  @moduledoc """
  Configex protocol use to mange configurations
  """

  @callback get(name :: atom) :: {:ok, any} | {:error, any} | no_return
  @callback get!(name :: atom) :: any | no_return
  @callback put(name :: atom, any) :: :ok | {:error, any} | no_return
  @callback put!(name :: atom, any) :: any | no_return
  @callback cast(name :: atom, any) :: {:ok, any} | {:error, any} | no_return
  @callback cast!(name :: atom, any) :: any | no_return
  @callback changed() :: :ok
  @callback changed!() :: :ok | {:error, any}

  defmacro __using__(opts) do
    quote do
      @behaviour Configex
      use Configex.Builder, unquote(opts)
    end
  end

end
