defplugin :control do
  defrecord State, identified: []

  def init(_options) do
    { :ok, State[] }
  end

  def handle(Event.Message[user: "cocks", content: "Mana: asl?"] = msg, _state) do
    msg.reply "12/f/cali u?"

    { :ok, _state }
  end

  def handle(Event.Message[user: "cocks", content: "Mana: wanna cyber?"] = msg, _state) do
    msg.reply "I take off my robe and wizard hat."

    { :ok, _state }
  end
end
