defrecord Mana.User, nick: nil, user: nil, host: nil do
  def parse(string) do
    [nick, rest] = string |> String.split("!")
    [user, host] = rest   |> String.split("@")

    Mana.User[nick: nick, user: user, host: host]
  end
end
