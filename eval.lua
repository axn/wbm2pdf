#!/usr/bin/lua

print("start:")

-- if truncTime (ms) is specified and ping TIME statistic is available then
-- the array of ping records will be truncated respectively.
-- Eg: if truncTime=10000 ping TIME=15000 then the last third of the
-- records in the array will be cut off
--
-- afterwards, if minSeq is specified then the array of ping records will
-- be extended again with NA values (reflecting a lost ping reply) so that
-- its size has at least minSeq entries

local in_log_files={
	{["file"]="test_data/mobile_node0/screenlog.ping_babel",          ["exp"]=1, ["minSeq"]=3860},
	{["file"]="test_data/mobile_node0/screenlog.ping_batadv",         ["exp"]=1, ["minSeq"]=3860},
	{["file"]="test_data/mobile_node0/screenlog.ping_bmx6",           ["exp"]=1, ["minSeq"]=3860},
	{["file"]="test_data/mobile_node0/screenlog.ping_olsr",           ["exp"]=1, ["minSeq"]=3860},

	{["file"]="test_data/mobile_node1/screenlog.ping_babel",          ["exp"]=2, ["minSeq"]=5220},
	{["file"]="test_data/mobile_node1/screenlog.ping_batadv",         ["exp"]=2, ["minSeq"]=5220},
	{["file"]="test_data/mobile_node1/screenlog.ping_bmx6",           ["exp"]=2, ["minSeq"]=5220},
	{["file"]="test_data/mobile_node1/screenlog.ping_olsr",	          ["exp"]=2, ["minSeq"]=5220},

	{["file"]="test_data/mobile_running_test0/screenlog.ping_babel",  ["exp"]=3, ["minSeq"]=840},
	{["file"]="test_data/mobile_running_test0/screenlog.ping_batadv", ["exp"]=3, ["minSeq"]=840},
	{["file"]="test_data/mobile_running_test0/screenlog.ping_bmx6",   ["exp"]=3, ["minSeq"]=840},
	{["file"]="test_data/mobile_running_test0/screenlog.ping_olsr",	  ["exp"]=3, ["minSeq"]=840},

	{["file"]="test_data/random_ping_test0/screenlog.0",              ["exp"]=4, ["minSeq"]=1000, ["truncTime"]=10000 },
	}

local out_data_file="./tmp.data"
local out_stat_file="./tmp.stat"

local protocols={ ["olsr"]   = "fdba:11:",
		  ["babel"]  = "fdba:14:",
		  ["batadv"] = "fdbb::",
		  ["bmx6"]   = "fdba:12:"
		}

local in_begin_keyword1="PING .* data bytes"
local in_end_keyword1="Terminated"

local in_failure_keyword="ping: sendmsg: Network is unreachable"

local min_pings=400



local out_data_fd=io.open(out_data_file,"w+")
out_data_fd:write("SEQ PROTO NODE RTT TTL GRP EXP LOGFILE".."\n")

local out_stat_fd=io.open(out_stat_file,"w+")


local stat_hdr_format="%-7s %3s %-6s %-3s %-4s %-16s %4s %4s %4s %5s %-6s %-4s %-3s %-4s %-4s %-6s %-6s %-6s %-6s %-6s %s \n"
local stat_lin_format="%-7s %3s %-6s %-3s %-4s %-16s %4s %4s %4s %5s %-6s %-4s %-3s %-4s %-4s %-6s %6.2f %-6s %-6s %-6.1f %s \n"

out_stat_fd:write(string.format(stat_hdr_format,
		  "PROTO", "EXP", "NODE", "GRP", "SIZE", "ADDR",
		  "SEND", "RCVD", "LOSS", "TIME",
		  "MAXSEQ", "UNIQ", "DUP", "LOST", "REOR",
		  "TTLMIN", "TTLAVG", "RTTMIN", "RTTMAX", "RTTAVG", "LOGFILE"))


local function init_ping()
	return {
		beg_key_found=false,
		file=nil,
		exp=nil,
		prot=nil,
		node_id=nil,
		bytes=nil,
		addr=nil,
		max_seq=0,
		last_seq=0,
		min_ttl=nil,
		max_ttl=nil,
		tot_ttl=0,
		min_time=nil,
		max_time=nil,
		tot_time=0,
		double_succeeds=0,
		unique_succeeds=0,
		reorders_last=0,
		reorders_max=0,
		unmatched=0,
		failure_after_succeeds=0,
		lost=0,
		pstats = nil,
		data_table = {}
	}
end

local function tableLength(t)
	assert(type(t)=="table")
	local c=0
	local n
	for n in pairs(t) do
		c=c+1
	end
	return c
end

local global_data = { prev_nodeid=nil, prev_exp=nil, curr_group=0, curr_pings=nil }

local function eval_pings(ping )
	
	if false and ping and ping.file then
		print("Summary: log=".. ping.file .." bytes="..(ping.bytes or "NA") .. " addr="..(ping.addr or "NA") ..
		      " max_seq="..ping.max_seq.."/"..(ping.unique_succeeds+ping.lost) ..
		      " unique="..ping.unique_succeeds .." double="..ping.double_succeeds ..
		      " failure="..ping.unmatched.."/"..ping.failure_after_succeeds.. " lost="..ping.lost..
		      " reorders="..ping.reorders_last.."/"..ping.reorders_max..
		      " ttl="..(ping.min_ttl or "NA") .. "/"..(ping.max_ttl or "NA").."/"..(ping.tot_ttl/ping.unique_succeeds)..
		      " rtt="..(ping.min_time or "NA").. "/"..(ping.max_time or "NA").."/"..(ping.tot_time/ping.unique_succeeds) )
	end
	
	if type(ping)=="table" and type(tonumber(ping.max_seq))=="number" and ping.max_seq >= min_pings then
		
		if global_data.prev_nodeid ~= ping.node_id or global_data.prev_exp ~= ping.exp then

			global_data.prev_nodeid = ping.node_id
			global_data.prev_exp = ping.exp
			global_data.curr_group = global_data.curr_group + 1
			global_data.curr_pings = {}
		end
		
		global_data.curr_pings[ping.prot] = ping
		
		if tableLength(global_data.curr_pings) == tableLength(protocols) then
			
			for p,a in pairs(protocols) do
				assert( type(global_data.curr_pings[p])=="table" )
				
				local ping = global_data.curr_pings[p]
				
				out_stat_fd:write(string.format(stat_lin_format,
						  ping.prot,
						  ping.exp,
						  ping.node_id,
						  global_data.curr_group,
						  (ping.bytes or "NA"),
						  (ping.addr or "NA"),
						  (ping.pstats and ping.pstats.transmitted or "NA"),
						  (ping.pstats and ping.pstats.received or "NA"),
						  (ping.pstats and ping.pstats.loss_percent or "NA"),
						  (ping.pstats and ping.pstats.time_ms or "NA"),
						  ping.max_seq,
						  ping.unique_succeeds,
						  ping.double_succeeds,
						  --ping.unmatched,
						  ping.lost,
						  ping.reorders_last,
						  (ping.min_ttl or "NA"),
						  --(ping.max_ttl or "NA"),
						  (ping.tot_ttl/ping.unique_succeeds),
						  (ping.min_time or "NA"),
						  (ping.max_time or "NA"),
						  (ping.tot_time/ping.unique_succeeds),
						  ping.file
						  ) )


				for i = 1,ping.max_seq do
					if ping.data_table[i] then
						out_data_fd:write(i.." "..ping.prot.." "..
								  ping.node_id.." "..
								  ping.data_table[i].time.." "..
								  ping.data_table[i].ttl.." "..
								  global_data.curr_group.." "..
								  ping.exp.." "..
								  ping.file.." "..
								  "\n")
					else
						ping.lost = ping.lost+1
						out_data_fd:write(i.." "..ping.prot.." "..
								  ping.node_id.." "..
								  "NA".." "..
								  "NA".." "..
								  global_data.curr_group.." "..
								  ping.exp.." "..
								  ping.file.." "..
								  "\n")
			
					end
				end
			end
		end
	end
	
	return init_ping()
end

local function iterate_log_file( log )

	--local curr_prot
	--
	--for p,i in pairs(protocols) do
	--	if in_log_file:match(p) then
	--		assert(not curr_prot)
	--		curr_prot = in_log_file:match(p)
	--	end
	--end
	--assert(curr_prot)
	

	
	local line
	local curr_line=0
	local ping = init_ping()

	
	for line in io.lines(log.file) do
		
		curr_line = curr_line + 1
		
		if string.match( line, in_begin_keyword1 ) then
			ping = eval_pings(ping)
			ping.beg_key_found=true
		end
	
		if string.match( line, in_end_keyword1) then
			ping = eval_pings(ping)
		end
		
		if ping.beg_key_found and ping.addr then
			
			if line:match( " "..ping.addr.." ping statistics ") then
				
				ping.pstats = { line=curr_line }
				
			elseif type(ping.pstats)=="table" then
				
				if ping.pstats.line==(curr_line - 1) and
				line:match( "transmitted") and line:match("received") and line:match("loss") then
					
					ping.pstats.transmitted  = ((line:match("[%d]+ packets transmitted")) or "NA"):match("[%d]+")
					ping.pstats.received     = ((line:match("[%d]+ received"))            or "NA"):match("[%d]+")
					ping.pstats.loss_percent = ((line:match("[%d]+%% packet loss"))       or "NA"):match("[%d]+")
					ping.pstats.time_ms      = ((line:match("time [%d]+ms"))              or "NA"):match("[%d]+")
				end
				
				ping = eval_pings(ping)
			end
		end
	
		if ping.beg_key_found then
			
			local probe_bytes = (line:match( "[%d]+ bytes") or "NA" ):match("[%d]+")
			local probe_addr  = (line:match( "from .*: ") or "NA" ):gsub("from ",""):gsub(": ","")
			local probe_seq   = (line:match( "icmp_seq=[%d]+") or "NA" ):match("[%d]+")
			local probe_ttl   = (line:match( "ttl=[%d]+") or "NA" ):match("[%d]+")
			local probe_time  = (line:match( "time=[%d]+.[%d]+ ms") or "NA" ):match("[%d]+.[%d]+")
			
			if (probe_bytes and probe_addr and probe_seq and probe_ttl and probe_time) then
				
				assert((not ping.file) or (ping.file==log.file))
				ping.file = log.file

				assert((not ping.exp) or (ping.exp==log.exp))
				ping.exp = log.exp

				
				for p,a in pairs(protocols) do
						
					assert( type(p)=="string" and type(a)=="string" and type(probe_addr)=="string" )
					
--					print( "probe_addr="..probe_addr.." a="..a)
					
					if probe_addr:match( "^"..a ) then
						
						local node_id = probe_addr:gsub(a,""):gsub("::1","")
						assert( (not ping.node_id) or (ping.node_id==node_id))
						assert( (not ping.prot) or (ping.prot==p))
						
						if (not ping.prot) then
							ping.node_id = node_id
							ping.prot = p
						end
					end
				end
				assert(ping.prot and ping.node_id)

				assert((not ping.bytes) or probe_bytes:match(ping.bytes), line)
				ping.bytes = ping.bytes and ping.bytes or probe_bytes
				
				assert((not ping.addr) or (ping.addr==probe_addr), log.file.." "..line)
				ping.addr = probe_addr
	
				probe_bytes = tonumber(probe_bytes)
				probe_seq = tonumber(probe_seq)
				probe_ttl = tonumber(probe_ttl)
				
				ping.max_seq = math.max((ping.max_seq or probe_seq),probe_seq)
				ping.min_ttl = math.min((ping.min_ttl or probe_ttl),probe_ttl)
				ping.max_ttl = math.max((ping.max_ttl or probe_ttl),probe_ttl)
				ping.tot_ttl = ping.tot_ttl + probe_ttl
				ping.min_time = math.min((ping.min_time or probe_time),probe_time)
				ping.max_time = math.max((ping.max_time or probe_time),probe_time)
				ping.tot_time = ping.tot_time + probe_time
				
	
				if ping.data_table[probe_seq] then
					ping.double_succeeds = ping.double_succeeds + 1
					print("file="..ping.file.." line="..curr_line .. " DOUBLE: " .. line)
	
				else
					--print( curr_line .. " UNIQUE:" ..
					--	" bytes="..probe_bytes .. " addr="..probe_addr ..
					--	" seq="..probe_seq ..
					--	" ttl="..probe_ttl ..
					--	" time="..probe_time ..
					--	"")
	
					ping.unique_succeeds = ping.unique_succeeds + 1
					ping.failure_after_succeeds = 0
					
					ping.reorders_last = (ping.last_seq > probe_seq) and (ping.reorders_last + 1) or ping.reorders_last
					ping.last_seq = probe_seq
					ping.reorders_max = (ping.max_seq > probe_seq) and (ping.reorders_max + 1) or ping.reorders_max
					
					ping.data_table[probe_seq] = {seq=probe_seq, ttl=probe_ttl, time=probe_time}
	
				end
		
			else
				--print(curr_line .. " UNMATCHED: " .. line)
				ping.unmatched = ping.unmatched + 1
				ping.failure_after_succeeds = ping.failure_after_succeeds + 1
			end
		end
	end
	
	ping = eval_pings(ping)
	
end






for i,p in ipairs(in_log_files) do
	iterate_log_file(p)
end


out_data_fd:close()

out_stat_fd:close()

for line in io.lines(out_stat_file) do print(line) end