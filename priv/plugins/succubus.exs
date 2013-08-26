defplugin :succubus do
  require Exquisite
  alias   Data.Dict

  def init(_options) do
    :timer.send_interval DateTime.to_seconds(minutes: 30) * 1000, :trigger
  end

  def info(:trigger, _state) do
    Enum.each Plugin.call(:logger, :list), fn { server, channel } ->
      query = Exquisite.match msg in Plugin.Logger.Message[in: { server, channel }],
        where: msg.in.server == server and msg.in.channel == channel

      messages = Enum.reduce Plugin.call(:logger, { :query, query }), HashDict.new, fn msg, acc ->
        Dict.update acc, msg.user.nick, [], &[msg.content | &1]
      end

      danger = Enum.map messages, fn { user, messages } ->
        { result, { percent, _, _ } } = messages |> Enum.reverse
          |> Enum.map(&String.split(&1, %r/[^a-zA-Z']+/))
          |> List.flatten
          |> weight()

        danger = if result == :female do
          cond do
            percent > 90 -> :ultra
            percent > 80 -> :high
            percent > 70 -> :medium
            percent > 60 -> :low
            true         -> :none
          end
        else
          :none
        end

        { user, danger }
      end

      unless Enum.empty?(danger) do
        { user, threat } = Enum.max_by danger, fn { _, danger } ->
          case danger do
            :none   -> 0

            :low    -> 1
            :medium -> 2
            :high   -> 3
            :ultra  -> 4
          end
        end

        if threat != :none do
          alert(server, channel, user, threat)
        end
      end
    end

    { :ok, _state }
  end

  defp alert(server, channel, user, danger) do
    message = case danger do
      :ultra ->
        "Hide your mana, we have a hungry succubus here, her name starts with #{user |> String.slice(0, 1) |> String.upcase}"

      :high ->
        "Succubus powers increasing, stay alert."

      :medium ->
        "I have the feeling a succubus is among us."

      :low ->
        "There might be a succubus among us, be careful."
    end

    Mana.Connection.send "PRIVMSG #{channel} :#{message}", to: server
  end

  @dictionary [
    { "actually", -49 },
    { "am", -42 },
    { "as", 37 },
    { "because", -55 },
    { "but", -43 },
    { "ever", 21 },
    { "everything", -44 },
    { "good", 31 },
    { "has", -33 },
    { "him", -73 },
    { "if", 25 },
    { "in", 10 },
    { "is", 19 },
    { "like", -43 },
    { "more", -41 },
    { "now", 33 },
    { "out", -39 },
    { "since", -25 },
    { "so", -64 },
    { "some", 58 },
    { "something", 26 },
    { "the", 17 },
    { "this", 44 },
    { "too", -38 },
    { "well", 15 }
  ]

  defp weight(words) when length(words) < 300 do
    { :unknown, { 0, 0, 0 } }
  end

  defp weight(words) do
    { male, female } = Enum.reduce words, { 0, 0 }, fn word, { male, female } ->
      weight = Dict.get(@dictionary, String.downcase(word), 0)

      if weight > 0 do
        { male  + weight, female }
      else
        { male, female - weight }
      end
    end

    percent = if male + female > 0 do
      female * 100.0 / (male + female)
    else
      0
    end

    stats = { percent, male, female }

    cond do
      percent == 0 ->
        { :unknown, stats }

      male > female ->
        { :male, stats }

      true ->
        { :female, stats }
    end
  end
end
