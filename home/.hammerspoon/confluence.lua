local utils = require('utils')
local confluenceAccount = require ('confluenceAccount')

local log = hs.logger.new('confluence.lua', 'debug')

-- Public part
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

-- Private part

return confluence

