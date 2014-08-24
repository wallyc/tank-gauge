
-record(tank, {
	id,						% tank number
	side = 0,  				% gauge height
	center = undefined,   	% center height
	safe = 0,				% safe height
	critical = 0,    		% critical zone-chart (interpolate fraction)
	strap = 0,    			% gpi
	insulated = true,   	% insulated: true|false?
	table = seven,   		% vcf table
	product,   				% product
	grav = 5.0				% API gravity @ 60F
  	}).

-record(gauge, {
	id,						% timestamp
	tank,					% tank
	hatch = side,			% side|center
	cgauge,					% converted gauge
	gauge_ht,				% gauge height
	safe_ht,				% safe fill height
	api,					% api
	table,					% table
	gauger,					% gauger
	gauge,					% gauge
	type = innage,			% ullage or innage
	gross,					% gross volume
	gross_x_scf,			% gross * shell expansion factor
	net,					% net volume
	temp = 60.0,			% tank temperature
	ambient,				% anbient temperature
	product,				% product
	grav = 10.0,			% API gravity @ 60
	scf = 1.00000,			% shell expansion factor
	vcf = 1.00000,			% volume correction factor
	aspace					% available space 
	}).

-record(sess, {
	id,
	user,
	passwd
	}).

-record(user, {
	id,
	passwd
	}).

-record(product, {
	id,
	name
	}).

-define(SIXTEENTH, 0.0625).
-define(BBL, 42).
