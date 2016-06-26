local utils = require('utils')
local jiraAccount = require ('jiraAccount')

local log = hs.logger.new('init.lua', 'debug')

-- Public part
local jira = {}

-- Returns a Jira URL to browse the given issue Key
function jira.getBrowseUrl(key)
   return string.format("%s%s%s", jiraAccount.getBaseUrl(), "browse/", key)
end

-- Returns a Jira URL to log work onto given issue id
function jira.getLogWorkUrl(id)
   return string.format("%s%s%s", jiraAccount.getBaseUrl(), "secure/CreateWorklog!default.jspa?id=", id)
end

-- Type JIRA issue browsing base url
function jira.typeBrowseUrl()
   local url = jira.getBrowseUrl(utils.getTrimmedSelectedText())
   hs.eventtap.keyStrokes(url)
end

-- Type a JIRA bug template
function jira.typeBugTemplate()
   local source=[[
# *Steps to reproduce*
  ##
  ##
# *Expected result*
  ##
  ##
# *Actual*
  ##
  ##
# *Extra information*
  ##
  ##
]]
   hs.eventtap.keyStrokes(source)
end

-- Search the highlighted selection in Request.jira.com
function jira.search()
   local url = string.format("%s%s%s%s", jiraAccount.getBaseUrl(), "issues/?jql=text%20~%20%22", utils.getTrimmedSelectedText(), "%22")
   log.f("Searching '%s'", url)
   -- TODO: if empty, pop-up a chooser
   utils.browseUrl(url)
end

-- Browse the issue key currently highlighted selection, or pop up a chooser
function jira.browseIssue()
   local key = utils.getTrimmedSelectedText()
   if key == "" then
      log.f("browseIssue: no selection: invoking UI")
      lookupJiraIssue()
   else
      log.f("browseIssue: browse issue '%s'", key)
      utils.browseUrl(jira.getBrowseUrl(key))
   end
end

-- Log work for given issue id in browser
function jira.logWork(id)
   utils.browseUrl(jira.getLogWorkUrl(id))
end

-- Private part

-- Below from https://github.com/CasperKoning/dothammerspoon/blob/master/jira.lua

-- Jira viewer: (also see https://developer.atlassian.com/jiradev/jira-apis/jira-rest-apis/jira-rest-api-tutorials/jira-rest-api-version-2-tutorial)
function createAuthorisationRequestBody()
   return string.format('{ "username": "%s", "password": "%s" }', jiraAccount.getUsername(), jiraAccount.getPassword())
end

function getSession()
   log.f("getSession(): entering")
   data = createAuthorisationRequestBody()
   headers = {["Content-Type"] = "application/json"}
   status, body, returnedHeaders = hs.http.post(jiraAccount.getBaseUrl() .. 'rest/auth/latest/session', data, headers)
   log.f("getSession(): received status %s, body '%s', headers '%s'", status, body, headers)
   if status == 200 then
      result = hs.json.decode(body)
      session = result["session"]
      return session["name"], session["value"]
   else
      return nil, nil
   end
end

function lookupJiraIssue()
   sessionName, sessionValue = getSession()
   if (sessionName ~= nil and sessionValue ~= nil) then
      log.f("lookupJiraIssue(): got a valid session.")
      local cookieHeaders = {["cookie"] = sessionName .. "=" .. sessionValue, ["Content-Type"] = "application/json"}

      local picker = hs.chooser.new(function(userInput)
	    if userInput ~= nil then
	       log.f("chooser: user chose '%s'",userInput)
	       if userInput["key"] ~= Nil then
		  local url = jira.getBrowseUrl(userInput["key"])
		  log.f("chooser: user chose '%s', browsing to '%s'", userInput["key"], url)
		  hs.execute("open " .. url)
	       end
	    end
      end)

      picker:queryChangedCallback(
	 function(query)
	    log.f("queryChangedCallback(): query is '%s'", query)
	    if string.len(query) > 3 then
	       log.f("queryChangedCallback(): query '%s' could be a valid JIRA issue key", query)
	       hs.http.asyncGet(
		  getJiraQueryUrl(query),
		  cookieHeaders,
		  function(status, body, headers)
		     log.f("queryChangedCallback(): received status %s, body '%s', headers '%s'", status, body, headers)
		     if status == 200 then
			searchResult = hs.json.decode(body)
			if searchResult["fields"] ~= nil then
			   local results = {}
			   local key = searchResult["key"]
			   local summary = searchResult["fields"]["summary"]
			   table.insert(results, {text = key, subText = summary, key = key})
			   picker:choices(results)
			end
		     end
		  end
	       )
	    else
	       log.f("queryChangedCallback(): query '%s' cannot be a valid JIRA issue key", query)
	    end
	 end
      )
      picker:rows(1)
      picker:show()
   else
      log.f("lookupJiraIssue(): could not get a valid session.")
      notify = hs.notify.new()
      notify:title("Jira")
      notify:informativeText("Could not get authorization")
      notify:send()
   end
end

function getJiraQueryUrl(query)
   local url = string.format("%s%s%s", jiraAccount.getBaseUrl(), "rest/api/latest/issue/", query)
   log.f("jiraQuey(): return url '%s'", url)
   return url
end

return jira
