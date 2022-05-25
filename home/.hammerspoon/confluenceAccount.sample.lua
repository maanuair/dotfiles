-- Copyright © 2016, 2019, 2022 Emmanuel Roubion
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

-- In order for the Hammerspoon connection to Confluence to work, copy this
-- file, rename it to exclude its suffix ".sample", and provide your own
-- settings. Again, do not version this resulting file which will contain
-- your credentials!

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

-- Returns the Confluence search URL
function confluenceAccount.getSearchUrl()
   return "https://undefined-SetMeFirst"
end

return confluenceAccount
