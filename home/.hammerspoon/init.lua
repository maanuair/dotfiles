local utils = require('utils')
local jira = require('jira')
local confluence = require('confluence')
local oblique_strategies = require('oblique_strategies')
local log = hs.logger.new('init.lua', 'debug')

-- Starts fresh
hs.console.setConsole()
log.i("Fresh Hammerspoon's config loaded !")

-- Inits for key bindings...
local m1 = {"cmd", "alt", "ctrl"}
local m2 = {"cmd", "alt", "ctrl", "shift"}

-- General hot keys
hs.hotkey.showHotkeys(m1,"H")
hs.hotkey.bind(m1, "D", "Dictionary: define highlighted selection (or ask)", utils.openDict)
hs.hotkey.bind(m1, "G", "Google: search the highlighted selection", utils.googleSelection)
hs.hotkey.bind(m1, "T", "Google: translate the highlighted selection", utils.googleTranslateSelection)
-- JIRA hot keys
hs.hotkey.bind(m1, "O", "JIRA: Immediate or interactive JIRA issue lookup", jira.browseIssue)
hs.hotkey.bind(m1, "J", "JIRA: Search highlighted selection", jira.search)
hs.hotkey.bind(m2, "J", "JIRA: Type the generic JIRA URL 'browse issue', and append highlighted selection", jira.typeBrowseUrl)
hs.hotkey.bind(m2, "B", "JIRA: Type a bug template", jira.typeBugTemplate)
-- Confluence hot keys
hs.hotkey.bind(m1, "C", "Confluence: Search highlighted selection", confluence.search)
-- Oblique Strategies hot keys
hs.hotkey.bind(m2, "O", "Show an Oblique Strategy", oblique_strategies.showStrategy)

-- Timer attempts

function remind()
  hs.notify.new({
    title = "Reminder",
	  informativeText = "Don't you remind ?\nTTD + PPL + MPC!",
  }):send()
end
a = hs.fs.attributes('~/.hammerspoon.timer')
if a == nil then
  log.df("No timer set.")
else
  hs.hotkey.bind(m1, 	"R", "Reminder", remind)
  hs.timer.doEvery(60*15 , remind)
  log.df("Reminder timer is set.")
  remind()
end
