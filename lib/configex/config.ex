defmodule Configex.Config do
  @moduledoc """
  Module to keep metadata for a configuration
  """

  @type t :: %__MODULE__{
    name: atom,
    type: atom,
    default: any,
    module: module,
    validator: {atom, any} | atom | nil,
    line: integer,
    file: String.t
  }

  @enfored_keys ~w{module name type line file}a
  defstruct [:module, :name, :type, :line, :file, :validator, default: nil]

  def call_validator({method, opts}, module, value) do
    apply(module, method, [value, opts])
  end

  def call_validator(method, module, value) when is_atom(method) do
    apply(module, method, [value])
  end

end