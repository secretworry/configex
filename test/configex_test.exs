defmodule ConfigexTest do
  use ExUnit.Case

  alias Configex.TestAdapter

  defmodule ConfigValue do
    defstruct key: nil
  end

  defmodule SampleConfig do
    use Configex, otp_app: :configex, adapter: Configex.TestAdapter
    config :string_config, :string, default: "default_string"
    config :integer_config, :integer, default: 5
    config :float_config, :float, default: 1.0
    config :list_config, :list
    config :map_config, :map
    config :struct_config, ConfigValue, default: %ConfigValue{}
    config :string_validator_config, :string, validator: fn "a" -> :ok end
  end

  setup_all do
    SampleConfig.start_link
    :ok
  end

  setup do
    TestAdapter.reset
    SampleConfig.changed!
  end

  describe "__configex__" do
    test "should export __configex__(:adapter)" do
      assert SampleConfig.__configex__(:adapter) == {Configex.TestAdapter, []}
    end

    test "should export __configex__(:config, type)" do
      for name <- ~w{string_config integer_config float_config list_config map_config struct_config string_validator_config}a do
        %{name: ^name} = SampleConfig.__configex__(:config, name)
      end
    end

    test "should export __configex__(:configs)" do
      assert SampleConfig.__configex__(:configs)
    end
  end

  describe "get/1" do
    test "should get default value" do
      assert "default_string" == SampleConfig.get!(:string_config)
      assert 5                == SampleConfig.get!(:integer_config)
      assert 1.0              == SampleConfig.get!(:float_config)
      assert []               == SampleConfig.get!(:list_config)
      assert %{}              == SampleConfig.get!(:map_config)
      assert %ConfigValue{}   == SampleConfig.get!(:struct_config)
    end
  end

  describe "put/2" do
    test "should put value" do
      assert :ok == SampleConfig.put(:string_config, "a")
      assert SampleConfig.get!(:string_config) == "a"
      assert TestAdapter.get(:string_config, []) == {:ok, "a"}
    end

    test "should raise error for setting an invalid value" do
      {:error, "integer_config should be a integer but got a"} = SampleConfig.put(:integer_config, "a")
      {:error, "float_config should be a float but got a"} = SampleConfig.put(:float_config, "a")
    end
  end

  describe "cast/2" do
    test "should cast and put a value" do
      assert {:ok, 1} == SampleConfig.cast(:integer_config, "1")
      assert SampleConfig.get!(:integer_config) == 1
      assert TestAdapter.get(:integer_config, []) == {:ok, 1}
    end
  end
end
