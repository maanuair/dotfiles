-- This file contains sensitive information, such as names, emails, phone number and address
-- NOT to be versionned, of course.

-- In order for the Hammerspoon utils library to work, copy this file
-- (without the .sample) and provide your own settings

local utilsAccount = { }

-- Returns the address for this account
function utilsAccount.getAddress()
   return "undefined-SetMeFirst"
end

-- Returns the email for this account
function utilsAccount.getEmail()
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

-- Returns the phone number  for this account
function utilsAccount.getPhoneNumber()
   return "undefined-SetMeFirst"
end

return utilsAccount
