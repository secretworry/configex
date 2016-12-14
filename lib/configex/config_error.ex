defmodule Configex.ConfigError do
  @moduledoc """
  Raise at runtime when error happend in sending request
  """
  defexception [:message]
end