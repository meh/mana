defserver "irc.wizardchan.org", plugins: [:control, :logger, :url] do
  nick     "Mana"
  user     "Mana"
  realname "Succubus begone."

#  defchannel "#wiz",  plugins: [:succubus, :regex]
  #defchannel "#anon", plugins: [:anonchat]
  #defchannel "#ask",  plugins: [:ask]

  defchannel "#code", plugins: [:succubus, :regex]
end
