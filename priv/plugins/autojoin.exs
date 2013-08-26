defplugin :autojoin do
  def handle(Event.Numeric[number: num, server: server], _state) when num in [422, 376] do
    Enum.each server.channels, fn { name, _ } ->
      server.send "JOIN #{name}"
    end

    { :ok, _state }
  end
end
