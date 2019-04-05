function initialize() {
  var myOptions = {
    zoom: 6,
    center: new google.maps.LatLng(-33, 151.5),
    mapTypeId: google.maps.MapTypeId.HYBRID
  }

  var map = new google.maps.Map(document.getElementById("map_canvas"),
                                myOptions);

  infowindow = new google.maps.InfoWindow({
	            maxWidth: 200
                });

  setMarkers(map, moorings);

}

/**
 * Data for the markers consisting of a name, a LatLng and a zIndex for
 * the order in which these markers should display on top of each
 * other.
 */
var moorings = [
 ['SHMO', -33.841, 151.264, 1, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">SHMO</h1>'+
        '<div id="bodyContent">'+
        '<p>The <b>S</b>ydney <b>H</b>arbour <b>M</b>arine <b>O</b>bservatory Realtime Buoy <b>SHMO</b> is located 0.4nm Southwest of the Sow and Pigs Reef between the Western and Eastern Channel.</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/realtime.html">'+
        'Link to realtime data</a></p>'+
        '</div>'+
        '</div>'],


  ['CH070', -30.275, 153.300, 1, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">CH070</h1>'+
        '<div id="bodyContent">'+
        '<p>The Coffs Harbour mooring <b>CH070</b> is located in 70 m water depth</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/nsw-imos/CH070_latest.html">'+
        'Plot of 12 Month Data</a></p>'+
        '</div>'+
        '</div>'],
  ['CH100', -30.268, 153.397, 2, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">CH100</h1>'+
        '<div id="bodyContent">'+
        '<p>The Coffs Harbour mooring <b>CH100</b> is located in 100 m water depth</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/nsw-imos/CH100_latest.html">'+
        'Plot of 12 Month Data</a></p>'+
        '</div>'+
        '</div>'],
  ['ORS065', -33.898, 151.315, 3, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">ORS065</h1>'+
        '<div id="bodyContent">'+
        '<p>The Sydney Water Ocean Reference Station mooring <b>ORS065</b> is located in 65 m water depth</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/nsw-imos/ORS065_latest.html">'+
        'Plot of 12 Month Data</a></p>'+
		'<p><a href="http://www.oceanography.unsw.edu.au/realtime.html">'+
        'Plot of Realtime Data</a></p>'+
        '</div>'+
        '</div>'],
  ['SYD100', -33.943, 151.382, 4, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">SYD100</h1>'+
        '<div id="bodyContent">'+
        '<p>The Sydney mooring <b>SYD100</b> is located in 100 m water depth</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/nsw-imos/SYD100_latest.html">'+
        'Plot of 12 Month Data</a></p>'+
        '</div>'+
        '</div>'],
  ['SYD140', -33.994, 151.495, 5, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">SYD140</h1>'+
        '<div id="bodyContent">'+
        '<p>The Sydney mooring <b>SYD140</b> is located in 140 m water depth</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/nsw-imos/SYD140_latest.html">'+
        'Plot of 12 Month Data</a></p>'+
        '</div>'+
        '</div>'],
  ['PH100', -34.120, 151.224, 6, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">PH100</h1>'+
        '<div id="bodyContent">'+
        '<p>The Port Hacking NRS mooring <b>PH100</b> is located in 100 m water depth</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/nsw-imos/PH100_latest.html">'+
        'Plot of 12 Month Data</a></p>'+
        '</div>'+
        '</div>'],

		  ['BMP070', -36.19, 150.19, 9, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">BMP070</h1>'+
        '<div id="bodyContent">'+
        '<p>The Batemans Marine Park mooring <b>BMP070</b> is located in 70 m water depth</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/nsw-imos/BMP070_latest.html">'+
        'Plot of 12 Month Data</a></p>'+
        '</div>'+
        '</div>'],

  ['BMP090', -36.192, 150.233, 7, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">BMP090</h1>'+
        '<div id="bodyContent">'+
        '<p>The Batemans Marine Park mooring <b>BMP090</b> was located in 90 m water depth</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/nsw-imos/BMP090_latest.html">'+
        'Plot of 12 Month Data</a></p>'+
        '</div>'+
        '</div>'],
  ['BMP120', -36.213, 150.309, 8, '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">BMP120</h1>'+
        '<div id="bodyContent">'+
        '<p>The Batemans Marine Park mooring <b>BMP120</b> is located in 120 m water depth</p>'+
        '<p><a href="http://www.oceanography.unsw.edu.au/nsw-imos/BMP120_latest.html">'+
        'Plot of 12 Month Data</a></p>'+
        '</div>'+
        '</div>'],
];

function setMarkers(map, locations) {
  // Add markers to the map

  for (var i = 0; i < locations.length; i++) {
    var mooring = locations[i];
    var myLatLng = new google.maps.LatLng(mooring[1], mooring[2]);
    var marker = new google.maps.Marker({
        position: myLatLng,
        map: map,
        title: mooring[0],
        zIndex: mooring[3],
        html: mooring[4]
    });

    google.maps.event.addListener(marker, 'click', function() {
          infowindow.setContent(this.html);
          infowindow.open(map,this);
	      });
  }
}
