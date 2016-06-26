-- This file contains sensitive environment information, such as passwords.
-- NOT to be versionned, of course.

-- In order for the Hammerspoon connection to JIRA to work, copy this file
-- (without the .sample) and provide your own settings

local jiraAccount = {}

-- Returns the username for this account
function jiraAccount.getUsername()
   return "undefined-SetMeFirst"
end

-- Returns the password for this account
function jiraAccount.getPassword()
   return "undefined-SetMeFirst"
end

-- Returns the Jira base URL
function jiraAccount.getBaseUrl()
   return "https://undefined-SetMeFirst"
end

return jiraAccount

