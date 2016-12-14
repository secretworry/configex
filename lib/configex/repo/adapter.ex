defmodule Configex.Repo.Adapter do

  @type opts :: Keyword.t

  @callback init(opts) :: any

  @callback get(atom, pid, any) :: {:ok, any} | {:error, any}

  @callback get(atom, any) :: {:ok, any} | {:error, any}

  @callback put(atom, any, any) :: :ok | {:error, any}

end