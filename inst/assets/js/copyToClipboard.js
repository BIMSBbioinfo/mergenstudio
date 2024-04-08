$(document).on('click', '.btn-clipboard', function(event) {

  // get the parent div of the parent div and next pre tag
  const parentDiv = $(this).closest('div').parent();
  const preTag = parentDiv.next('pre');

  // find code inside pre tag
  const codeTag = preTag.find('code');
  const code = codeTag.text();

  // create temp textarea and copy the code inside it
  const tempInput = $('<textarea>').val(code);
  $('body').append(tempInput);
  tempInput.select();
  document.execCommand('copy');
  tempInput.remove();

  // update clipboard button text
  const codeButton = $(this);
  const originalContent = codeButton.html();
  codeButton.html('Copied!');

  // reset clipboard button text after 1 second
  setTimeout(function() {
    codeButton.html(originalContent);
  }, 1000);
});


$(document).on('click', '.btn-exec', function(event) {

  // get the closest child div with class "assistant-message-wrapper"
  const parentDiv = $(this).closest('div').parent();
  const closestAssistantDiv = parentDiv.next('div.assistant-message-wrapper');

  const textinside = closestAssistantDiv[0].innerText.trim();

  // set the value for Shiny input
  Shiny.setInputValue("app-chat-responsejs", textinside, { priority: "event" });

  // Search for pre tags with a copy button within the closest assistant div
  // copy button indicates that it is actual code and not some output
  // which we would not want to run
  const preTagsWithCopyButton = closestAssistantDiv.find('pre.hasCopyButton');

  //get all code
  const code = preTagsWithCopyButton.map(function() {
    const codeTagText = $(this).find('code').text().trim();
    return codeTagText;
  }).get();

  // set the value for Shiny input
  Shiny.setInputValue("app-chat-codejs", code, { priority: "event" });

  // update clipboard button text
  const codeButton = $(this);
  const originalContent = codeButton.html();
  codeButton.html('Running code!');

  // reset clipboard button text after 1 second
  setTimeout(function() {
    codeButton.html(originalContent);
  }, 1000);
});





// gpt-created
function addCopyBtn() {
  // Get all the pre tags in the document that don't already have a copy button
  var preTags = $('pre:not(.hasCopyButton):not(.DoesntNeedCopy)');

  // Loop through all the pre tags
  preTags.each(function() {
    // Add class to indicate that the copy button has been added
    $(this).addClass('hasCopyButton');

    // Get the code element inside pre tag and its language class
    const codeTag = $(this).find('code');
    var language = codeTag.attr('class');
    if (language == undefined) {
      language = 'output';
    }

    // Create a div element with the copy button and language text
    // The svg icon was generated using FontAwesome library via R
    var div = $(`
    <div class="d-flex justify-content-between bg-dark" style="border-radius: 5px 5px 0 0">
    <p class="px-2 py-1 m-0 text-muted small">${language}</p>
    <div>
        <button type="button" class="btn action-button btn-secondary btn-sm btn-clipboard shiny-bound-input ml-auto">
            <i class="fas fa-copy"></i> Copy
        </button>
    </div>
    </div>
    `);
    // Insert the div with the copy button and language text before the pre tag
    $(this).before(div);
  });
}

function addExecButton() {
  var AssistantDivs = $('div.assistant-message-wrapper:not(.hasExecButton)');

  AssistantDivs.each(function() {
    // Check if the current AssistantDiv contains a <code> tag
    if ($(this).find('pre.hasCopyButton').length > 0) {
      $(this).addClass('hasExecButton');
      var div = $(`
        <div class="d-flex justify-content-between bg-dark" style="border-radius: 5px 5px 0 0">
          <div>
            <button type="button" class="btn action-button btn-secondary btn-sm btn-exec shiny-bound-input ml-auto">
              <i></i> Execute response
            </button>
          </div>
        </div>
      `);

      // Insert the div with the execute button before the current div
      $(this).before(div);
    }
  });
}




$(document).on('shiny:inputchanged', function(event) {
  // DO NOT CHANGE THE ORDER OF THIS! FIRST COPY button
  // Then exec button so the execbutton can see where there are
  // answers with actual code chunks in it and not output stuff.
  addCopyBtn();
  addExecButton();
});
