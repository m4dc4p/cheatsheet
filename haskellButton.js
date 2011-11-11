/* USAGE EXAMPLE
<style>
	.example { padding:15px; border: 2px solid #888; width:100px;float:left;margin-left: 5px; font-size: 50px; margin-top: 50px; }
</style>
<div id="example1" class="example">1</div>
<div id="example2" class="example">2</div>
<div id="example3" class="example">3</div>
<div id="example4" class="example">4</div>
<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"></script>
	<script type="text/javascript" src="/javascript/robotButton.js"></script>
	<script>
		$(document).ready(function() {
			initHaskellButton($('#example1'));
			initHaskellButton($('#example2'));
			initHaskellButton($('#example3'), function() { $('#example3').css("color","green");} );
			initHaskellButton($('#example4'), function() { $('#example4').css("color","gold");} );
			});
		</script>
*/
function initHaskellButton(item,clicked_fn) {
	var clicked,
	  detectDist = 90,
	  popupHeight = 70,
	  item_loc = item.offset(),
    item_dim = { h: item.height(), w: item.width() },
    itemDetect = $('<span></span>'),
    lilrobot = $('<div></div>');

	lilrobot.css({ "z-index": 100, 
                 "background-position": "0px -540px", 
                 "position": "absolute", 
                 "background-image": "url(codeslower_sprites_sm.png)", 
                 "font-size": "0em", 
                 "padding": popupHeight + "px 75px 0px 0px", 
                 "background-repeat": "no-repeat" });

  itemDetect.css({
    "z-index": 100,
    "position": "absolute",
    "left": item_loc.left - detectDist,
    "top": item_loc.top - detectDist,
    "width": item_dim.w + detectDist * 2,
    "height": item_dim.h + detectDist * 2
  });

	itemDetect.mousemove(function(e) {
		if(!clicked) {
			var item_center = { y: item_loc.top + (item_dim.h / 2), x: item_loc.left + (item_dim.w / 2) }
			lilrobot.offset({top: item_loc.top - popupHeight, 
                       left: item_loc.left + (item_dim.w / 2) - 32});
			var distY = Math.abs(e.pageY - item_center.y) - (item_dim.h/2);
			var distX = Math.abs(e.pageX - item_center.x) - (item_dim.w/2);
			var dist = (distY < distX ? distX : distY);

			if(distX < detectDist && distY < detectDist)
				lilrobot.css("background-position", "0px " + ((dist/detectDist)*popupHeight) + "px");
			else
				lilrobot.css("background-position", "0px -540px");
		}
	});

  // Always hide the sprite when the mouse leaves the bounding box.
  itemDetect.mouseleave(function(e) {
		lilrobot.css("background-position", "0px -540px");
  });

	item.click(function(e) {
		clicked = true;
		lilrobot.css("background-position", "-80px 0px");
		if(typeof clicked_fn == "function")
			clicked_fn();
	});

	item.after(lilrobot);
  $('body').append(itemDetect);
}

/*
Copyright (c) 2011 Joshua Siler, Justin Bailey

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

