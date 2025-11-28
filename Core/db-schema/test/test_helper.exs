# Start the repository in test mode
ExUnit.start()

# Setup the database sandbox for concurrent testing
Ecto.Adapters.SQL.Sandbox.mode(WpPraxis.Repo, :manual)
