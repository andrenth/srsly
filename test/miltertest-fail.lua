-- Echo that the test is starting
mt.echo("*** begin test")
-- start the filter
binpath = mt.getcwd() .. "/_build/src"
daemon = "srslyd.native"
filter = binpath .. "/" .. daemon

mt.echo("*** executing " .. filter)
mt.startfilter(filter, "test/srslyd.conf")
mt.sleep(2)

conn = "inet:8387@127.0.0.1"
envfrom = "spf-test@digirati.com.br"
helofqdn = "mta90.f1.k8.com.br"
heloaddr = "187.73.32.145"

conn = mt.connect(conn)
if conn == nil then
  error "mt.connect() failed"
end

if mt.conninfo(conn, helofqdn, heloaddr) ~= nil then
  error "mt.conninfo() failed"
end
if mt.getreply(conn) ~= SMFIR_CONTINUE then
  error "mt.conninfo() unexpected reply"
end
if mt.mailfrom(conn, envfrom) ~= nil then
  error "mt.mailfrom() failed"
end
if mt.getreply(conn) ~= SMFIR_CONTINUE then
  error "mt.mailfrom() unexpected reply"
end

-- end of message; let the filter react
if mt.eom(conn) ~= nil then
  error "mt.eom() failed"
end
if mt.getreply(conn) ~= SMFIR_CONTINUE then
  error "mt.eom() unexpected reply"
end

if not mt.eom_check(conn, MT_HDRINSERT, "Authentication-Results") then
  error "no Authentication-Results header added"
end

if not mt.eom_check(conn, MT_HDRINSERT, "Received-SPF") then
  error "no header added"
end

mt.disconnect(conn)
