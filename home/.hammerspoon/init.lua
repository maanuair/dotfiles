-- Copyright © 2016, 2017, 2018, 2019, 2020 Emmanuel Roubion
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
-- Hammerspoon entry point

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

-- m1 Shortcuts
hs.hotkey.showHotkeys(m1,"H")
hs.hotkey.bind(m1, "A", "Type my address", utils.typeAddress)
hs.hotkey.bind(m1, "B", "Browse the highlighted selection", utils.openBrowser)
hs.hotkey.bind(m1, "C", "Confluence search the highlighted selection", confluence.search)
hs.hotkey.bind(m1, "D", "Define (in Dictionary) the highlighted selection (or ask)", utils.openDict)
hs.hotkey.bind(m1, "E", "Type my email", utils.typeEmail)
hs.hotkey.bind(m1, "F", "Type my first name", utils.typeFirstName)
hs.hotkey.bind(m1, "G", "Google the highlighted selection", utils.googleSelection)
hs.hotkey.bind(m1, "J", "Jira search the highlighted selection", jira.search)
hs.hotkey.bind(m1, "L", "Type my lastname", utils.typeLastName)
hs.hotkey.bind(m1, "M", "Toggle menu bar", utils.toggleMenuBar)
hs.hotkey.bind(m1, "T", "Translate with Google the highlighted selection", utils.googleTranslateSelection)
hs.hotkey.bind(m1, "O", "Open highlighted Jira issue (or ask)", jira.browseIssue)
hs.hotkey.bind(m1, "P", "Type my phone number", utils.typePhoneNumber)
hs.hotkey.bind(m1, "Q", "Qwant the highlighted selection", utils.qwantSelection)

-- m2 Shortcuts
hs.hotkey.bind(m2, "B", "Type a Bug scenario template, in Jira mark-up style", jira.typeBugTemplate)
hs.hotkey.bind(m2, "E", "Type my alt. email", utils.typeAltEmail)
hs.hotkey.bind(m2, "J", "Jira generic URL is typed (highlighted selection appended)", jira.typeBrowseUrl)
hs.hotkey.bind(m2, "O", "Oblique Strategy pop-up!", oblique_strategies.showStrategy)
hs.hotkey.bind(m2, "S", "Type a Story scenario template, in Jira mark-up style", jira.typeStoryTemplate)

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
