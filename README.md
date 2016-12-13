# Configex

  Mange type-specified configurations for elixir

# Quick Example

  ```elixir
    # Define the configuration module
    defmodule MyApp.Configex do
      use Configex, otp_app: :my_app
      
      config :integer_config, :integer, default: 5
      config :string_config, :string, default: "default_value"
      config :struct_config, MyApp.UserPreferences, default: UserPreference.new
    end
    
    # use Configex
    # get a configuration
    MyApp.Configex.get(:integer_config)
    # set a configuration, and persist it to a repo
    MyApp.Configex.put(:string_config, "new_value")
    
    # set a value of invalid type will make the call fail
    MyApp.Configex.put(:struct_config, "not_an_valid_value")
    
    # cast a input from user
    MyApp.Configex.cast(:struct_config, %{"enable_notification" => false})
  ```
