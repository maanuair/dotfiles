-- This file contains sensitive environment information, such as passwords.
-- NOT to be versionned, of course.

-- In order for the Hammerspoon connection to Confluence to work, copy this file
-- (without the .sample) and provide your own settings

local confluenceAccount = {}

-- Returns the username for this account
function confluenceAccount.getUsername()
   return "undefined-SetMeFirst"
end

-- Returns the password for this account
function confluenceAccount.getPassword()
   return "undefined-SetMeFirst"
end

-- Returns the Confluence base URL
function confluenceAccount.getBaseUrl()
   return "https://undefined-SetMeFirst"
end

return confluenceAccount

