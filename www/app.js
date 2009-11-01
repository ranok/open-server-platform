$(function() { 
    $(".draggable").draggable({
       	helper: 'clone',
	connectToSortable: ".sortable"
    });

    $(".droppable").sortable({
        connectWith: '.droppable',
        stop: function(event, ui) {
		if ($(ui.item).children("a").size() == 0) {
		    $(ui.item).append('<br /><br /><a href="#" class="delete_app">Delete</a>');
		}
	}
    });

    $("body").click(function(event) {
	var $target = $(event.target);
        if ($target.is(".delete_app")) {
	    event.preventDefault();
	    $target.parent().remove();
	}
    });
});