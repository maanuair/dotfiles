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
  -- Accordig to the issue key, we use either the base or alt URL
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

-- Search the highlighted selection in Request.jira.com
function jira.search()
  local url = jiraAccount.getBaseUrl() .. "issues/?jql=project IN " .. jiraAccount.getDefaultSearchProjects() .. " AND summary ~ \"" .. utils.getTrimmedSelectedText() .. "\" ORDER BY issueKey DESC"
  log.f("Searching '%s'", url)
  -- TODO: if empty, pop-up a chooser
  utils.browseUrl(url)
end

-- Browse the issue key currently highlighted selection, or pop up a chooser
function jira.browseIssue()
  local key = utils.getTrimmedSelectedText()
  if string.len(key) == 0 then
    log.f("browseIssue: no selection: invoking graphical chooser")
    lookupJiraIssue()
  else
    -- Does the key starts with only a digit ?
    local c1 = string.sub(key, 1, 1)
    if string.match(c1, "^%d") ~= nil then
      -- Yes: add the default project prefix !
      log.f("browseIssue: first char '%s' is a digit, adding prefix '%s'", c1, jiraAccount.getDefaultProjectPrefix())
      key = jiraAccount.getDefaultProjectPrefix() .. key
    end
    log.f("browseIssue: browse issue '%s'", key)
    utils.browseUrl(jira.getBrowseUrl(key))
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
