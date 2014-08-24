$(function(){
		var feet = range(1, 50);
		var inches = range(1, 11);
    $('#mobiscroll').scroller({
        wheels: [ // Wheel groups array
    [ // First wheel group
        { // Wheel object
            label: 'Feet', 
            keys: feet,  
            values: feet, 
        }, 
        { // Wheel object
            label: 'Inches', 
            keys: inches,  
            values: inches, 
        },
        { // Wheel object
            label: 'Fraction', 
            keys: ['0.0','0.0625','0.125','0.1875','0.25','0.3125','0.375','0.4375','0.5','0.5625','0.625','0.6875','0.75','0.8125','0.875','0.9375'],  
            values: ['none','1/16','1/8','3/16','1/4','5/16','3/8','7/16','1/2','9/16','5/8','11/16','3/4','13/16','7/8','15/16']
        } 
    ], 
] ,
				display: 'inline',
				height: '20'
    });    
});


function range(start, end) {
    var foo = [];
    for (var i = start; i <= end; i++) {
        foo.push(i);
    }
    return foo;
}
