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

-- Returns the alternative Jira base URL, the one to use when the issueKey does not match the one returned by getDefaultProjectPrefix()
function jiraAccount.getAltBaseUrl()
   return "https://undefined-SetMeFirst"
end

-- Returns the Jira projects in which to perform searches queries.
-- Must be a list of comma separated project labels, included in parenthesis, to be used in a "project in" JQL clause.
-- E.g.: return "'Undefined Project One', 'Undefined Project Two'"
-- NB: use simple quote to delimit the project name(s)
function jiraAccount.getDefaultSearchProjects()
   return "('Undefined Project One\', 'Undefined Project Two')"
end


-- Returns the default search string to set in the Jira issue chooser. Can be empty.
function jiraAccount.getDefaultIssueSearch()
   return "defaultSearchInputToSet"
end

-- Returns the prefix for the default JIRA project
function jiraAccount.getDefaultProjectPrefix()
   return "defaultProjectPrefix"
end

return jiraAccount
