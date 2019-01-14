hs.osascript = require("hs.osascript")

local utils = {}

-- Private part

-- Our private logger instane
local log = hs.logger.new('utils.lua', 'debug')

-- Reload automatically the config
function reloadConfig(files)
   doReload = false
   for _,file in pairs(files) do
      if file:sub(-4) == ".lua" then
	 doReload = true
	 break
      end
   end
   if doReload then
      hs.reload()
      local s = "Hammerspoon config on disk was changed and reloaded!"
      hs.alert.show(s)
      log.df(s)
   end
end
hs.pathwatcher.new("~/.homesick/repos/dotfiles/home/.hammerspoon/", reloadConfig):start()

-- Public part
--------------

-- A trim function, c.f. http://lua-users.org/wiki/StringTrim
function utils.trim (s)
   return (s:gsub("^%s*(.-)%s*$", "%1"))
end

-- Capture the trimmed selected text
function utils.getTrimmedSelectedText()
   return utils.trim(utils.getSelectedText())
end

-- Look up current selection in dictionary
function utils.openDict()
   local uri = "dict://" .. utils.getTrimmedSelectedText()
   log.f("Open '%s'", uri)
   utils.browseUrl(uri)
end

-- Browse the  current selection
function utils.openBrowser()
   local uri = utils.getTrimmedSelectedText()
   log.f("Browse '%s'", uri)
   utils.browseUrl(uri)
end

-- Open the given URI in Safari
function utils.browseUrl (url)
   log.df("browseUrl(\"%s\")", url)
   hs.execute("open \"" .. url .. "\"")
end

-- Google the highlighted selection
function utils.googleSelection ()
  local text = utils.getTrimmedSelectedText()
  log.f("Captured the text '%s'", text)
  local uri = "https://www.google.fr/search?q=" .. text
  log.f("Browse '%s'", uri)
  utils.browseUrl(uri)
end

-- Google translate the highlighted selection
function utils.googleTranslateSelection ()
  local uri = "https://translate.google.fr/#view=home&op=translate&sl=fr&tl=en&text="
  local text = utils.getTrimmedSelectedText()
  log.f("URI is '%s'", uri)
  log.f("Captured the highlighted text '%s'", text)

  -- Run the JS encodeURIComponent on the captured text
  local js = "encodeURIComponent('" .. text .. "')"
  log.f("Running js code: %s", js)
  local status, object, descriptor = hs.osascript.javascript(js)
  if status == true then text = object end

  -- local js = "encodeURI('" .. uri .. "')"
  -- log.f("Running js code: %s", js)
  -- status, object, descriptor = hs.osascript.javascript(js)
  -- if status == true then uri = object end

  utils.browseUrl(uri .. text)
end

-- Wait a few millis seconds. Yes, hack.
function utils.wait(ms)
   if ms == nil then ms = 250000 end
   log.df("Now waiting %dms.", ms)
   hs.timer.usleep(ms)
   log.df("Wait over.")
end

-- Capture the currently highlighted/selected text, or clipboard content if empty
function utils.getSelectedText()
   -- Save the entire clipboard, for later restore
   local contentsBackup = hs.pasteboard.getContents()
   log.df("Clipboard saved (actually, is \"%s\").", contensBackup)

   -- Clears clipboard
   hs.pasteboard.clearContents()
   log.df("Cleared clipboard")
   utils.wait()

   -- Send copy command to capture current highlighted selection
   hs.eventtap.keyStroke({"cmd"}, "c")
   log.df("Sent CMD + C")
   utils.wait()

   -- Copy text from clipboard
   local selection = hs.pasteboard.readString()
   if selection == nil then
      selection = ""
   end
   log.df(string.format("Got highlighted selection \"%s\".", selection))

   -- Restore original content
   hs.pasteboard.setContents(contentsBackup)
   log.df("Restored original clipboard")

   -- Returns the precious information!
   log.df(string.format("Returning selection \"%s\"", selection))
   return selection
end

return utils
