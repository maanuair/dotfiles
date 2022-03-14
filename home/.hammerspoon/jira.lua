-- Copyright © 2016, 2017, 2018, 2019, 2020 Emmanuel Roubion
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
-- Some JIRA related helpers functions

-- Requirements and local vars
local utils = require('utils')
local jiraAccount = require ('jiraAccount')
local inspect = require('inspect')
local log = hs.logger.new('jira.lua', 'debug')
local jira = {}

local function startsWith(str, start)
  return str:sub(1, #start) == start
end

-- Returns a Jira URL to browse the given issue Key
function jira.getBrowseUrl(key)
  log.f("getBrowseUrl: Build url for issue key '%s'", key)
  local url = "";
  -- According to the issue key, we use either the base or alt URL
  key = string.upper(key)
  if string.len(key) == 0 or startsWith(key, jiraAccount.getDefaultProjectPrefix()) then
    -- It uses the ! above, so that when the key is empty, the test fails into the else :-)
    url = jiraAccount.getBaseUrl()
  else
    url = jiraAccount.getAltBaseUrl()
  end
  log.f("getBrowseUrl: use base URL '%s'", url)
  url = string.format("%sbrowse/%s", url, key)
  return url
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
# *Reproducibility*
  ## 100%
# *Traces*
  ## File attached foobar.log
  ## {code}Or traces captured directly pasted here, between "code" tag elements, maybe multi lines. {code}
  ## !Foobar Sreenshot.png|thumbnail!
# *Extra Information*
  ##
  ##
]]
  hs.eventtap.keyStrokes(source)
end

-- Type a JIRA story template, in JIRA mark-up style
function jira.typeStoryTemplate()
  local source=[[
*As a* <role> in <interface>
*I can* <do this>
*in order to* <get some result>
]]
  hs.eventtap.keyStrokes(source)
end

-- Search the highlighted selection in Request.jira.com
function jira.search()
  local url = jiraAccount.getBaseUrl() .. "/issues/?jql=text ~ '" .. utils.getTrimmedSelectedText() .. "' AND project IN " .. jiraAccount.getDefaultSearchProjects() .. " AND statusCategory in ('To Do', 'In Progress', Done) AND issueType IN (Epic, Story, Bug) ORDER BY project ASC, issueKey DESC"

  log.f("Searching '%s'", url)
  -- TODO: if empty, pop-up a chooser
  utils.browseUrl(url)
end

-- Return the JIRA project  matching the given issueKey
local function getProjectFromIssueKey(issueKey)
  result = nil
  defaultProject = nil
  projectFound = false
  -- Iterate over each JIRA server
  for p,project in ipairs(jiraAccount.getJiraProjects()) do
    log.f("getProjectFromIssueKey: iterating over JIRA server '%s'...", project["url"])
    for k,key in ipairs(project["keys"]) do
      log.f("getProjectFromIssueKey:     > iterating over project key '%s'...", key)
      if string.find(issueKey, key.."%-") ~= nil then
        log.f("getProjectFromIssueKey: issueKey '%s' matches key '%s' of server '%s'.", issueKey, key, project["url"])
        result = project
        projectFound = true
        break
      end
    end -- key
    if projectFound then
      break
    end
  end -- project

  -- Project not found ?
  if not projectFound then
    -- No project matched, so use the fall back if it exists
    log.f("getProjectFromIssueKey: could not find a matching project for issueKey '%s', check fall back project", issueKey)
    project = getProjectFromIssueKey("*-123")
    if project ~= nil then
      projectFound = true
    end
  end
  if not projectFound then
    log.f("getProjectFromIssueKey: could not match issue key '%s'.", issueKey)
  end
  return result
end

local function notifyJiraProjectNotFound(infoText)
  hs.notify.new({
      title = "JIRA project not found",
      informativeText = infoText
  }):send()
end

-- Return the URL to browse for the given project and issueKey, or nil when not found.
local function getUrlForProjectAndIssueKey(project, issueKey)
  result = nil
  -- Any project ?
  if project == nil
  then
    log.f("getUrlFromIssueKey: no project given with issueKey '%s'. Abort.", issueKey)
    notifyJiraProjectNotFound("No URL found for issueKey '".. issueKey .. "'. \nAborted. ")
  else
    result = project["url"] .. "/browse/" .. issueKey
  end
  return result
end

-- Browse the issue key currently highlighted selection, or pop up a chooser
function jira.browseIssue()
  local issueKey = utils.getTrimmedSelectedText()
  if string.len(issueKey) == 0 then
    log.f("browseIssue: no selection: invoking graphical chooser")
    lookupJiraIssue()
  else
    -- Transform to upper case, in case it is needed...
    issueKey = string.upper(issueKey)
    log.f("browseIssue: got selection '%s'", issueKey)
    -- Find the corresponding project
    project = getProjectFromIssueKey(issueKey)
    if project ~= nil then
      url = getUrlForProjectAndIssueKey(project, issueKey)
      log.f("browseIssue: browse issue '%s' at url '%s'.", issueKey, url)
      utils.browseUrl(url)
    else
      log.f("browseIssue: no matching project for issueKey '%s'", issueKey)
      notifyJiraProjectNotFound("No project for issue key '".. issueKey .. "'.")
    end
  end
end

-- Below from https://github.com/CasperKoning/dothammerspoon/blob/master/jira.lua

-- Jira viewer: (also see https://developer.atlassian.com/jiradev/jira-apis/jira-rest-apis/jira-rest-api-tutorials/jira-rest-api-version-2-tutorial)
function createAuthorisationRequestBody()
  log.f("createAuthorisationRequestBody(): entering")
  local s = '{ "username": "' .. hs.http.encodeForQuery(jiraAccount.getUsername()) .. '", "password": "' .. hs.http.encodeForQuery(jiraAccount.getPassword()) .. '" }'
  -- log.f("createAuthorisationRequestBody(): JSON auth payload is: \n%s", s)
  return s
end

function getSession()
  log.f("getSession(): entering")

  -- Prepare request
  local url = jiraAccount.getBaseUrl() .. 'rest/auth/latest/session'
  local auth = createAuthorisationRequestBody()
  local headers = { ["Content-Type"] = "application/json" }
  log.f("getSession(): requesting session as follows:")
  log.f("    url:     %s", url)
  log.f("    payload: %s", auth)
  log.f("    headers: %s", inspect(headers))

  -- Perform request
  status, body, returnedHeaders = hs.http.post(jiraAccount.getBaseUrl() .. 'rest/auth/latest/session', auth, headers)
  log.f("getSession(): request returned:")
  log.f("    status:  %s", status)
  log.f("    body:    %s", body)
  log.f("    headers: %s", inspect(returnedHeaders))

  -- Check result
  if status == 200 then
    -- Parse result
    local json = hs.json.decode(body)
    log.f("getSession(): got result: %s", inspect(json))

    -- Extract useful part
    session = json["session"]
    return session["name"], session["value"]
  else
    return nil, nil
  end
end

function clearTable (t)
  while #t ~= 0 do rawset(t, #t, nil) end
end

function lookupJiraIssue()
  sessionName, sessionValue = getSession()
  if (sessionName ~= nil and sessionValue ~= nil) then
    log.f("lookupJiraIssue(): got a valid session.")
    local cookieHeaders = { ["cookie"] = sessionName .. "=" .. sessionValue, ["Content-Type"] = "application/json" }

    local picker = hs.chooser.new(function(userInput)
        -- When user chose an item in the proposed list
        if userInput ~= nil then
          log.f("chooser: user chose '%s'", inspect(userInput))
          if userInput["key"] ~= Nil then
            local url = jira.getBrowseUrl(userInput["key"])
            log.f("chooser: user chose '%s', browsing to '%s'", userInput["key"], url)
            hs.execute("open " .. url)
          end
        end
    end)

    picker:query(jiraAccount.getDefaultIssueSearch())

    local results = {}

    picker:queryChangedCallback(
      function(q)
        if q == jiraAccount.getDefaultIssueSearch() then
          -- Do nothing :-)
          clearTable(results)
          table.insert(results, { text = q, subText = "Please type a valid key...", key = q })
          picker:rows(1) -- Set, but UI will not update acordingly, c.f. https://github.com/Hammerspoon/hammerspoon/issues/1725
          picker:choices(results)
        elseif string.len(q) > 3 and string.match(string.sub(q, 4), "[^%d]") == nil then
          -- Search for a JIRA key
          log.f("queryChangedCallback(): search for query '%s'", q)
          hs.http.asyncGet(
            getJiraQueryUrl(q),
            cookieHeaders,
            function(status, body, headers)
              log.f("getSession(): request returned:")
              log.f("    status:  %s", status)
              log.f("    body:    %s", body)
              log.f("    headers: %s", inspect(headers))
              if status == 200 then
                json = hs.json.decode(body)
                if json["fields"] ~= nil then
                  key = json["key"]
                  summary = json["fields"]["summary"]
                else
                  key = "No result found (should not hapen !)"
                  summary = "Key " .. q .. "cannot be found"
                end
              else
                key = "HTTP status code " .. status
                summary = "Unexpected HTTP status code"
              end
              clearTable(results)
              table.insert(results, { text = key, subText = summary, key = key })
              picker:rows(1) -- Set, but UI will not update acordingly, c.f. https://github.com/Hammerspoon/hammerspoon/issues/1725
              picker:choices(results)
            end
          )
        else
          -- Search for non JIRA key: for now, we jsut don't search :)
          log.f("queryChangedCallback(): won't search for query '%s'", q)
          local summary = q .. " is not a valid key"
          clearTable(results)
          table.insert(results, { text = q, subText = summary, key = q })
          picker:rows(1) -- Set, but UI will not update acordingly, c.f. https://github.com/Hammerspoon/hammerspoon/issues/1725
          picker:choices(results)
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

function getJiraQueryUrl(q)
  local url = string.format("%s%s%s", jiraAccount.getBaseUrl(), "rest/api/latest/issue/", q)
  log.f("jiraQuey(): return url '%s'", url)
  return url
end

return jira
