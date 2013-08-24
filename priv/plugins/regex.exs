defplugin :regex do
  require Exquisite

  def handle(Event.Message[content: ":s/" <> rest, server: server, channel: channel, user: User[nick: nick]] = msg, _state) do
    destructure [matcher, replacer, options], String.split(rest, "/")

    case Regex.compile(matcher, options || "") do
      { :ok, regex } ->
        query = Exquisite.match msg in Plugin.Logger.Message[in: { server, channel }],
          where: msg.in.server == server.name and msg.in.channel == channel.name and
                 msg.date > DateTime.now |> DateTime.minus(minutes: 10)

        modified = Plugin.call(:logger, { :query, query }) |> Enum.reverse |> Enum.find_value fn msg ->
          unless msg.content |> String.starts_with? ":s/" do
            if Regex.match?(regex, msg.content) do
              Regex.replace(regex, msg.content, replacer) |> msg.content
            end
          end
        end

        if modified do
          msg.reply "<#{modified.user.nick}> #{modified.content}"
        else
          msg.reply "#{nick}, no match for your uniquiry."
        end

      { :error, { reason, column } } ->
        msg.reply "#{nick}, your regex is invalid kind sir: #{reason} at column #{column}"
    end

    { :ok, _state }
  end
end
