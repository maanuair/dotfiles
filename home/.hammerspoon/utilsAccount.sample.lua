-- Copyright © 2019, 2020, 2021 Emmanuel Roubion
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
-- This file contains sensitive information, such as names, emails, phone number and address
-- NOT to be versionned, of course.

-- In order for the Hammerspoon utils library to work, copy this file,
-- rename it to exclude its suffix ".sample", and provide your own
-- settings. Again, do not version this resulting file which will contain
-- your credentials!

local utilsAccount = { }

-- Returns the address for this account
function utilsAccount.getAddress()
   return "undefined-SetMeFirst"
end

-- Returns the email for this account
function utilsAccount.getEmail()
   return "undefined-SetMeFirst"
end

-- Returns the alt email for this account
function utilsAccount.getAltEmail()
   return "undefined-SetMeFirst"
end

-- Returns the first name
function utilsAccount.getFirstName()
   return "undefined-SetMeFirst"
end

-- Returns the last name for this account
function utilsAccount.getLastName()
   return "undefined-SetMeFirst"
end

-- Returns the phone number for this account
function utilsAccount.getPhoneNumber()
   return "undefined-SetMeFirst"
end

-- Returns the alt. phone number for this account
function utilsAccount.getPhoneNumber()
   return "undefined-SetMeFirst"
end

return utilsAccount
