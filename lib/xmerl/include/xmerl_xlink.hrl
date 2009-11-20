


%% The following is a brief summary of the element types (columns) on 
%% which the global attributes are allowed:
%%
%%	     simple  extended locator  arc   resource  title
%% type		X	X	X	X	X	X
%% href		X		X
%% role		X	X	X		X
%% title	X	X	X		X
%% show		X	X		X
%% actuate	X	X		X
%% from					X
%% to					X
%%
-record(xlink, {
		type,	% simple | extended | locator | arc | resource | title
		href,
		role
		title,
		show,
		actuate,
		from,
		to
	       }).
