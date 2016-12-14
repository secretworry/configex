defmodule Configex.TestAdapter do
  @behaviour Configex.Repo.Adapter

  def start_link do
    Agent.start_link(fn -> Map.new end, name: __MODULE__)
  end

  def init(opts), do: opts

  def get(key, _pid, opts), do: get(key, opts)

  def get(key, _opts) do
    {:ok, Agent.get(__MODULE__, fn map -> Map.get(map, key) end)}
  end

  def put(key, value, _opts) do
    Agent.update(__MODULE__, fn map -> Map.put(map, key, value) end)
    :ok
  end
end