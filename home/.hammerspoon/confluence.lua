-- Copyright © 2016, 2017, 2019 Emmanuel Roubion
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
-- This is some helper functions, probably to tune to use our own...

-- Requirements and local vars
local utils = require('utils')
local confluenceAccount = require ('confluenceAccount')
local log = hs.logger.new('confluence.lua', 'debug')

local confluence = {}

-- Search the highlighted selection in Confluence
function confluence.search()
   local url = string.format("%s%s%s", confluenceAccount.getBaseUrl(), "dosearchsite.action?queryString=", utils.getTrimmedSelectedText())
   log.f("confluence.search: opening url '%s'", url)
   utils.browseUrl(url)
end

-- Add an Epic in Confluence
function confluence.addEpic()
   local url = string.format("%s%s", confluenceAccount.getBaseUrl(), "pages/createpage-entervariables.action?templateId=8159257&spaceKey=PRD&title&newSpaceKey=PRD&fromPageId=4754503")
   log.f("confluence.addEpic: opening url '%s'", url)
   utils.browseUrl(url)
end

return confluence
