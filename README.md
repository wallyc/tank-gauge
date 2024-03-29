# tank-gauge

tank-gauge facilitates the calculation of petroleum storage tank volumes on mobile devices. 

The current UI is built on top of functionality specific to the [YAWS](http://yaws.hyber.org) web server. Measurement data is persisted in Mnesia. It does not implement OTP behaviours, does not function properly in Internet Explorer and no effort has been made yet to implement a responsive layout for the desktop. 

## It supports the following:

* Upright cylindrical tanks with flat, concave, or convex bottoms
* Strappings in either tabular or theoretical (gpi) format
* Innage and ullage measurements
* Side and center hatch gauge points
* Critical zones requiring fraction interpolation
* Volume correction in algorithmic or tabular format (examples of tables 6b & 7 are included) 
* Shell correction (table B-1) for insulated or uninsulated tanks
* Calculation of stopgauges for receipts and discharges
* Quick view of last measurement for a tank (see note)
* View tank metrics/info.

Note: Actually, all meaurement data is persisted and accessible via other means but only the last measurement is typically useful to the gauger in the field. 

## Installation

Install [YAWS](http://yaws.hyber.org).

cd into your yaws/www directory, get and build the code:

```
	git clone git://github.com/wallyc/tank-gauge.git
	cd gauge
	make
```

Start YAWS in interactive mode and prepare the database (use the yaws command path specific to your installation):

```
	./bin/yaws -i --mnesiadir db
	db:stop().
	db:init().
	db:start().
	db:load_data().
```

## Getting Started

Browse to http://localhost:8000/tank-gauge/index.yaws (or whatever host name and port you configured yaws to use) and login using the demo credentials: user="Demo" and password="demo1234". 

Demo data has been included and the app's use should be discoverable and self-explanatory.




