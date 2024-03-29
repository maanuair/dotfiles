-- Copyright © 2016, 2018, 2019, 2020, 2021, 2022, 2023 Emmanuel Roubion
--
-- Author: Emmanuel Roubion
-- URL: https://github.com/maanuair/dotfiles

-- This file is part of Emmanuel's Roubion dot files, released under
-- the MIT License as published by the Massachusetts Institute of Technology
--
-- These dotfiles are distributed in the hope they wil lbe useful, but
-- without any warranty. See the MIT License for more details
--
-- You should have received a copy of the MIT License along with this file.
-- If not, see https://opensource.org/licenses/mit-license.php

-- 8<-----

-- Requirements and local vars
local utilsAccount = require ('utilsAccount')
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
function utils.openBrowser ()
  local uri = utils.getTrimmedSelectedText()
  log.f("Browse '%s'", uri)
  utils.browseUrl(uri)
end

-- Open the given URI in Safari
function utils.browseUrl (url)
  log.df("browseUrl(\"%s\")", url)
  local cmd = string.format("open \"%s\"", url)
  hs.execute(cmd)
  log.df("hs.execute(\"%s\")", cmd)
end

-- Google the highlighted selection
function utils.googleSelection ()
  local text = utils.getTrimmedSelectedText()
  log.f("Captured the text '%s'", text)
  local uri = "https://www.google.fr/search?q=" .. text
  log.f("Browse '%s'", uri)
  utils.browseUrl(uri)
end

-- Qwant the highlighted selection
function utils.qwantSelection ()
  local text = utils.getTrimmedSelectedText()
  log.f("Captured the text '%s'", text)
  local uri = "https://org.qwant.com/?q=" .. text .. "&t=all"
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
    log.df("Selection is nil, use empty string")
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

-- Type my address
function utils.typeAddress()
  local s = utilsAccount.getAddress()
  log.df("Going to type address \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type my first name
function utils.typeFirstName()
  local s = utilsAccount.getFirstName()
  log.df("Going to type first name \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type my email
function utils.typeEmail()
  local s = utilsAccount.getEmail()
  log.df("Going to type email \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type my email
function utils.typeOtherEmail()
  local s = utilsAccount.getOtherEmail()
  log.df("Going to type other email \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type my alt. email
function utils.typeAltEmail()
  local s = utilsAccount.getAltEmail()
  log.df("Going to type email \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type my last name
function utils.typeLastName()
  local s = utilsAccount.getLastName()
  log.df("Going to type last name \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type my LinkedInProfileURL
function utils.typeLinkedInProfileURL()
  local u = utilsAccount.getLinkedInProfileURL()
  log.df("Going to type LinkedIn profile URL \"%s\"", u)
  hs.eventtap.keyStrokes(u)
end

-- Type my phone number
function utils.typePhoneNumber()
  local s = utilsAccount.getPhoneNumber()
  log.df("Going to type phone number \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type my phone number
function utils.typeAltPhoneNumber()
  local s = utilsAccount.getAltPhoneNumber()
  log.df("Going to type alt. phone number \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type current date
function utils.typeDate()
  local s = os.date("%Y-%m-%d")
  log.df("Going to type current date \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type current timestamp
function utils.typeTimestamp()
  local s = os.date("%Y%m%d%H%M%S")
  log.df("Going to type current timestamp \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Type current date
function utils.typeDate()
  local s = os.date("%Y-%m-%d")
  log.df("Going to type current date \"%s\"", s)
  hs.eventtap.keyStrokes(s)
end

-- Turn on/off macOS's menu bar
function utils.toggleMenuBar()
  -- Unforunately, the script is locale dependent! :-/
  hs.osascript.applescript([[
    tell application "System Preferences"
      activate
      set bounds of window 1 to {0, 0, 0, 0}
      set the current pane to pane id "com.apple.preference.general"
      delay 1
      tell application "System Events" to tell process "System Preferences"
        click checkbox "Masquer/afficher automatiquement la barre des menus" of window 1
      end tell
      quit
    end tell
    ]])
end

-- Open a new note in OneNote
function utils.newOneNote()
  log.df("yes!!")
  -- Unforunately, the script is locale dependent! :-/
  hs.osascript.applescript([[
    tell application "Microsoft OneNote" to activate
    delay 1
	  tell application "System Events"
		  click menu item "Nouvelle page" of ((process "OneNote")'s (menu bar 1)'s (menu bar item "Fichier")'s (menu "Fichier"))
	  end tell
    delay 1
    ]])
  utils.typeDate()
  hs.eventtap.keyStrokes(' ')
end

return utils
