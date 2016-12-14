defmodule Configex.Repo.Adapter do

  @type opts :: Keyword.t

  @callback init(opts) :: any

  @callback get(atom, pid, any) :: {:ok, any} | {:error, any}

  @callback get(atom, any) :: {:ok, any} | {:error, any}

  @callback put(atom, any, any) :: :ok | {:error, any}

  defmodule NonNotifiable do

    defmacro __using__(_) do
      quote do
        @behaviour Configex.Repo.Adapter
        def init(opts), do: opts

        def get(atom, pid, any), do: get(atom, any)

        defoverridable [init: 1]
      end
    end
  end
end