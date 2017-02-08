defmodule Configex.TestAdapter do
  use Configex.Repo.Adapter.NonNotifiable

  def start_link do
    Agent.start_link(fn -> Map.new end, name: __MODULE__)
  end

  def init(opts), do: opts

  def get(key, _opts) do
    {:ok, Agent.get(__MODULE__, fn map -> Map.get(map, key) |> maybe_decode end)}
  end

  def put(key, value, _opts) do
    Agent.update(__MODULE__, fn map -> Map.put(map, key, value |> Poison.encode!) end)
    :ok
  end

  defp maybe_decode(nil), do: nil
  defp maybe_decode(json) when is_binary(json), do: Poison.decode!(json)

  def reset do
    Agent.update(__MODULE__, fn _ -> %{} end)
    :ok
  end
end