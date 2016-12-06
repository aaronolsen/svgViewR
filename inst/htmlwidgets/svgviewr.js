HTMLWidgets.widget({

  name: 'svgviewr',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        var svg = document.createElement('svg_doc');
        el.appendChild(svg);
        svg.outerHTML = x.svg;
        
        Object.keys(x)
          .filter(function(d){return d !== "svg"})
          .map(function(d){
            window[d] = x[d];
          });
          
        onLoadFunctions();
        
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});