defmodule Configex.TypesTest do
  use ExUnit.Case

  alias Configex.Types

  defmodule ConfigValue do
    defstruct key: nil
  end

  describe "valida_type!/1" do
    test "should validate builtin_types without error" do
      for buildin_type <- ~w{string integer float map list}a do
        Types.validate_type!(buildin_type)
      end
    end

    test "should validate a struct module without error" do
      defmodule StructModule do
        defstruct key: nil
      end
      assert :ok == Types.validate_type!(StructModule)
    end

    test "should raise error for module not defining a struct" do
      defmodule SimpleModule do
      end
      assert_raise ArgumentError, fn ->
        Types.validate_type!(SimpleModule)
      end
    end
  end

  describe "validate_value/3" do
    test "should validate a string" do
      assert :ok == Types.validate_value(:string, "path", "string")
      assert {:error, "path should be a string but got 1"}
          == Types.validate_value(:string, "path", 1)
    end

    test "should validate a integer" do
      assert :ok == Types.validate_value(:integer, "path", 1)
      assert {:error, "path should be a integer but got string"}
          == Types.validate_value(:integer, "path", "string")
    end

    test "should validate a float" do
      assert :ok == Types.validate_value(:float, "path", 1.5)
      assert :ok == Types.validate_value(:float, "path", 1)
      assert {:error, "path should be a float but got string"}
          == Types.validate_value(:float, "path", "string")
    end

    test "should validate a list" do
      assert :ok == Types.validate_value(:list, "path", [])
      assert {:error, "path should be a list but got string"}
          == Types.validate_value(:list, "path", "string")
    end

    test "should validate a map" do
      assert :ok == Types.validate_value(:map, "path", %{})
      assert {:error, "path should be a map but got string"}
          == Types.validate_value(:map, "path", "string")
    end

    test "should validate a list of string" do
      assert :ok == Types.validate_value({:list, :string}, "path", [])
      assert :ok == Types.validate_value({:list, :string}, "path", ["value"])

      assert {:error, "path[1] should be a string but got 1"}
          == Types.validate_value({:list, :string}, "path", ["value", 1])
    end

    test "should validate a map of string" do
      assert :ok == Types.validate_value({:map, :string}, "path", %{})
      assert :ok == Types.validate_value({:map, :string}, "path", %{"key" => "string"})
      assert {:error, "path[int] should be a string but got 1"}
          == Types.validate_value({:map, :string}, "path", %{"key" => "string", "int" => 1})
    end
  end

  describe "cast/2" do
    test "should cast integer" do
      assert {:ok, 1} == Types.cast(:integer, 1)
      assert {:ok, 1} == Types.cast(:integer, "1")
      assert {:error, "expect a :integer but got 3.5"} == Types.cast(:integer, 3.5)
    end

    test "should cast string" do
      assert {:ok, "string"} == Types.cast(:string, "string")
      assert {:ok, "1"} == Types.cast(:string, 1)
      assert {:ok, "3.5"} == Types.cast(:string, "3.5")
      assert {:ok, "%{}"} == Types.cast(:string, %{})
    end

    test "should cast float" do
      assert {:ok, 3.5} == Types.cast(:float, 3.5)
      assert {:ok, 1.0} == Types.cast(:float, 1)
      assert {:ok, 3.5} == Types.cast(:float, "3.5")
    end

    test "should cast list" do
      assert {:ok, []} == Types.cast(:list, [])
      assert {:ok, ~w{a b c}} == Types.cast(:list, ~w{a b c})
      assert {:ok, [4]} == Types.cast(:list, 4)
      assert {:ok, [1.5]} == Types.cast(:list, 1.5)
      assert {:ok, ["string"]} == Types.cast(:list, "string")
      assert {:ok, ~w{a b c}} == Types.cast(:list, "a,b,c")
      assert {:error, "expect a :list but got %{}"} == Types.cast(:list, %{})
    end

    test "should cast map" do
      assert {:ok, %{}} == Types.cast(:map, %{})
      assert {:ok, %{"a" => "a", "b" => "b"}} == Types.cast(:map, [a: "a", b: "b"])
    end

    test "should cast a list of the same type" do
      assert {:ok, [1, 2, 3]} == Types.cast({:list, :integer}, ~w{1 2 3})
      assert {:ok, [1, 2, 3]} == Types.cast({:list, :integer}, "1,2,3")
      assert {:ok, ["1", "2", "3"]} == Types.cast({:list, :string}, ~w{1 2 3})
      assert {:ok, [1.0, 2.0, 3.0]} == Types.cast({:list, :float}, ~w{1 2 3})
      assert {:ok, [1.0, 2.0, 3.0]} == Types.cast({:list, :float}, "1.0, 2.0, 3.0")
      assert {:error, "expect a {:list, :integer} but got [\"a\", \"b\", \"c\"]"} == Types.cast({:list, :integer}, ~w{a b c})
    end

    test "should cast a map of the same type" do
      assert {:ok, %{"a" => 1, "b" => 2}} == Types.cast({:map, :integer}, %{a: "1", b: "2"})
      assert {:ok, %{"a" => "1", "b" => "2"}} == Types.cast({:map, :string}, %{a: "1", b: "2"})
    end

    test "should cast a struct" do

      assert {:ok, %ConfigValue{key: "value"}} == Types.cast(ConfigValue, %{key: "value"})
      assert {:ok, %ConfigValue{key: "value"}} == Types.cast(ConfigValue, %{"key" => "value"})
    end
  end
end