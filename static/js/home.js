// Main entry point
$(function(){
  setupFeatures();
});

// Expandable features
function setupFeatures(){
  $('.features .span6').each(function(){
    var $this = $(this);
    $this.click(function(){
      $this.find('.expandable').slideToggle(function(){
        $this.find('.expand').slideToggle('fast');
      });
    });
    if ($this.find('.expandable').size() == 0)
      $this.find('.expand').hide();
  });
}
