local utils = require('utils')
local jira = require('jira')
local confluence = require('confluence')

-- Starts fresh
hs.console.setConsole()

-- Logger stuffs
local log = hs.logger.new('init.lua', 'debug')
log.i("Fresh Hammerspoon's config loaded !")

-- Inits for key bindings...
local myModifiers = {"cmd", "alt", "ctrl"}
local myModifiersShift = {"cmd", "alt", "ctrl", "shift"}

-- General hot keys
hs.hotkey.showHotkeys(myModifiers, "H")
hs.hotkey.bind(myModifiers, "V", "Paste text only", utils.pasteTextOnly)
hs.hotkey.bind(myModifiers, "D", "Dictionary: define highlighted selection (or ask)", utils.openDict)

-- JIRA hot keys
hs.hotkey.bind(myModifiers, "B", "JIRA: Type a bug template", jira.typeBugTemplate)
hs.hotkey.bind(myModifiers, "O", "JIRA: Browse highlighted issue (or ask)", jira.browseIssue)
hs.hotkey.bind(myModifiers, "J", "JIRA: Search highlighted selection", jira.search)
hs.hotkey.bind(myModifiers, "T", "JIRA: Log Product Task work (TT-94 - Team)", function () jira.logWork("64369") end)
hs.hotkey.bind(myModifiers, "P", "JIRA: Log Product Process work (TT-95 - Process)", function () jira.logWork("64405") end)
hs.hotkey.bind(myModifiersShift, "J", "JIRA: type URL for highlighted issue", jira.typeBrowseUrl)

-- Confluence hot keys
hs.hotkey.bind(myModifiers, "C", "Confluence: Search highlighted selection", confluence.search)
hs.hotkey.bind(myModifiers, "E", "Confluence: Add an Epic in Product space.", confluence.addEpic)

-- Experimental below
function dlFinished(exitcode, stdout, stderr)
   utils.debug("Finished")
end

function dlOutputs()
   utils.debug("Outputs...")
end

function youtubeDL()
   local app = hs.application.frontmostApplication()
   local appName = app:name(win);
   utils.debug(string.format("Current application is \"%s\".", appName))
   if appName ~= "Firefox" and appName ~= "Safari" and appName ~= "Chrome" then
      hs.alert.show("Not a browser, shortcut ignored.")
   else
      hs.eventtap.keyStroke({"cmd"}, "l")
      local url = utils.getSelectedText()
      local pos = string.find(url, "https://www.youtube.")
      if pos == nil or pos ~= 1 then
	 hs.alert.show("Not on YouTube, shortcut ignored.")
      else
	 local cmd = string.format("/usr/local/bin/youtube-dl \"%s\" -x --metadata-from-title \"%%(artist)s - %%(title)s\" -o \"/Users/eroubion/Downloads/%%(title)s.%%(ext)s\" 2>&1", url)
	 utils.debug(string.format("Command to launch: \n%s", cmd))
	 local r = hs.execute(cmd)
	 utils.debug(string.format("Command returned: %s", r))
	 -- local args = {"" }
	 --t = hs.task.new("/usr/local/bin/youtube-dl", dlFinished(exitcode, stdout, stderr))
	 --t.start()
      end
   end
end
