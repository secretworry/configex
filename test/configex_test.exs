defmodule ConfigexTest do
  use ExUnit.Case

  defmodule SampleConfig do
    use Configex, otp_app: :configex, adapter: Configex.TestAdapter
    config :string_config, :string, default: "default_string"
    config :integer_config, :integer, default: 5
    config :float_config, :float, default: 1.0
  end
end
