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
    {:ok, 5} = MyApp.Configex.get(:integer_config)
    5 = MyApp.Configex.get!(:integer_config)
    
    # set a configuration, and persist it to a repo
    :ok = MyApp.Configex.put(:string_config, "new_value")
    
    # set a value of invalid type will make the call fail
    MyApp.Configex.put(:struct_config, "not_an_valid_value")
    
    # cast a input from user input
    {:ok, %MyApp.UserPreferences{enable_notification: false}} = MyApp.Configex.cast(:struct_config, %{"enable_notification" => false})
    {:ok, %MyApp.UserPreferences{enable_notification: false}} = MyApp.Configex.get(:struct_config)
  ```

# Usage

  1. Create an `Configex.Repo.Adapter` to handle config persistence
  
  2. Create a module `use Configex`, and define configs in this module
  
  3. Associate the adapter with the created `Configex` in the config file
  
    ```elixir
    # in config.exs
    config :my_app, MyApp.Configex,
      adapter: {MyApp.Configex.EctoRepo, repo: MyApp.Repo}
    ```
    
  4. Add the defined `Configex` which is a GenServer to the supervision tree of the Application, 
