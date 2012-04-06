

-record(property,{
			name,
			title,
			type=text, %%Property type (scalar,frequency,server,text,bool,numeric,schedule,check_group,password,hidden)
			description="",
			order=99,
			editable=true,
			configurable=true,
			advance=false,
			state=false,
			optional=false,
			default="",
			allowother=false, %%Whether to allow scalar attribute with the other input box
			listSize=1,			%%scalarProperty Use
			multiple=false,		%%scalar Property Use,Whether the multiple choice
			upIsBad=true,     %%For baseline, Is bigger and more bad value, For example: the greater the occupancy rate of more bad, but the remaining space is bigger, the better
			baselinable=false,     %%for baseline ,Some of these figures the return value types such as http 200 or 400, there can be a baseline of
			primarystate=false,
			properties=[]
			}).

-record(property_category,{
			name,		%%Baseline of the name attribute
			title,		%%Baseline of Property Title.
			properties	%%Property(list)
			}
		).