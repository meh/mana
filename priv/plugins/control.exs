defplugin :control do
  defrecord State, identified: []

  def init(_options) do
    { :ok, State[] }
  end

  def handle(event, _state) do
    IO.inspect event

    { :ok, _state }
  end
end
