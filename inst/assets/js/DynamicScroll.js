window.onload = function() {
    // Select the target node
    var targetNode = document.body;

    // Options for the observer (which mutations to observe)
    var config = { childList: true, subtree: true };

    // Callback function to execute when mutations are observed
    var callback = function(mutationsList, observer) {
        for(var mutation of mutationsList) {
            if (mutation.type === 'childList') {
                // Check if any added nodes are paragraphs
                mutation.addedNodes.forEach(function(node) {
                  if (node.className == 'row m-0 p-0 justify-content-end' ||
                      node.className == 'row m-0 p-0 justify-content-start' ){
                     // Scroll to the newly added paragraph
                        node.scrollIntoView({ behavior: 'smooth', block: 'end' ,inline: "center" });
                  }
                });
            }
        }
    };

    // Create an observer instance linked to the callback function
    var observer = new MutationObserver(callback);

    // Start observing the target node for configured mutations
    observer.observe(targetNode, config);
};

