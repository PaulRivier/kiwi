
function copyToClipboard(content) {
  var $temp = $("<input>");
  $("body").append($temp);
  $temp.val(content).select();
  document.execCommand("copy");
  $temp.remove();
}

$(document).ready(function() {

  var reload = $("ul.nav-items li a.reload");
  
  reload.on('click', function(e) {
	e.preventDefault();
	$.ajax({
      type: "POST",
      url: "/reload",
      data: "{}"
	}).done(function(data) {});
  });


  var edit_link = $("ul.nav-items li a.edit-page");

  edit_link.on('click', function(e) {
	e.preventDefault();
	$.ajax({
      type: "POST",
      url: "/edit-page",
      data: {"page-source": $(this).attr("data-page-source"),
             "page-id": $(this).attr("data-page-id")}
	}).done(function(data) {});
  });

  var copy_link = $("ul.nav-items li a.copy-link");

  copy_link.on('click', function(e) {
	var l = $(this).attr("data-page-link");
	copyToClipboard(l);
  });


  var logout = $(".layout-login a.logout");
  
  logout.on('click', function(e) {
	e.preventDefault();
	$.ajax({
      type: "POST",
      url: "/logout",
      data: "{}",
      complete: function(){ location.reload(); }
	}).done(function(data) { } );
  });


  $(".page-view .document figure img").on('click',function(e) {
    lity($(this).attr('src'));
	return false;
  });

  $(".page-view .document a.kiwi-link-image").on('click',function(e) {
    lity($(this).attr('href'));
	return false;
  });

  $("a.kiwi-link-file, a.kiwi-link-external").attr("target","_blank");


  var tocSetting = $(".js-toc").first().attr("data-setting");
  
  var docHeadings = $(".page-view .document h1," +
			          ".page-view .document h2," +
			          ".page-view .document h3");
  if ( tocSetting == "on" ||
       ( tocSetting == "auto" && docHeadings.length > 6) ) {
	tocbot.init({
	  // Where to render the table of contents.
	  tocSelector: '.js-toc',
	  // Where to grab the headings to build the table of contents.
	  contentSelector: '.page-view .document',
	  // Which headings to grab inside of the contentSelector element.
	  headingSelector: 'h1, h2, h3',
	  // For headings inside relative or absolute positioned containers within content.
	  hasInnerContainers: false,
	  orderedList: false
	});
  };

});
