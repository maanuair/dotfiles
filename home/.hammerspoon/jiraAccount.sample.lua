-- Copyright © 2016, 2019, 2020 Emmanuel Roubion
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
-- This file contains sensitive environment information, such as passwords.
-- NOT to be versionned, of course.

-- In order for the Hammerspoon connection to JIRA to work, copy this
-- file, rename it to exclude its suffix ".sample", and provide your own
-- settings. Again, do not version this resulting file which will contain
-- your credentials!

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

-- Returns the array of JIRA servers and projects we want in scope It
-- could be only one JIRA server, or several of them.
--
-- Note one project key (and only one) can be "*": it denotes the fall
-- back JIRA server to use when no keys match
--
-- The array has the following structure:
-- {
--   -- First server
--   {
--     keys={ "PRJ1", "PRJ2" } -- List of project prefixes living on this server
--     url="https://jira.domain.com" -- The URL for this JIRA server
--   },
--   -- Second server
--   {
--     keys={ "ABC", "*" } -- List of project prefixes living on this server
--     url="https://jira.another-domain.com" -- The URL for this JIRA server
--   }
-- }
function jiraAccount.getJiraProjects()
  return {
    {
      keys = { "*" },
      url = "https://jira.default-domain.com"
    }
  }
end

 return jiraAccount
