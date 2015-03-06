/*
  Bindings to ReactJS.

  To generate lib.js do:

     $ npm install
     $ grunt

 */


var React     = require('react');
var invariant = require('./invariant');

/**
 * Construct a new React.DOM node.
 *
 * @param {string} tag                 the tag you'd like to generate
 * @param {object} props               key-value list of object-properties
 * @param {ReactComponent[]} children  the node's children
 *
 * DANGER: children will be changed and MUST no be used anymore later.
 *
 */
function mkDomNode(tag, props, children) {
    var mkTag = React.DOM[tag];
    invariant(mkTag, 'Tried to construct unsupported ReactJS DOM node %s.', tag);
    children.unshift(props);
    return mkTag.apply(this, children);
}

var GhcjsApp = React.createClass({
        displayName: "GhcjsApp"
      , render:      function() {
                       // FIXME (SM): Undo this resetting of this or report
                       // this bug to GHCJS.
                       //
                       // Find a better way for a callback to return a
                       // value.
                       //
                       var smuggler = {};
                       this.props.onRender.apply(window, [smuggler]);
                       return smuggler.node;
      }
      // , componentWillMount   : function() { console.log("componentWillMount"); }
      // , componentWillUpdate  : function() { console.log("componentWillUpdate"); }
      // , componentWillUnmount : function() { console.log("componentWillUnmount"); }
      , componentDidMount : function() {
          this.props.onDidUpdate(this.getDOMNode());
      }
      , componentDidUpdate : function() {
          this.props.onDidUpdate(this.getDOMNode());
      }
      , shouldComponentUpdate : function(nextProps, nextState) {
          var smuggler = {};
          this.props.shouldUpdate.apply(window, [smuggler]);
          return smuggler.result;
      }
    });

var GhcjsAppFactory = React.createFactory(GhcjsApp);

function queryLifeCycleNode(rootNode, elemId) {
    return rootNode.querySelectorAll("[data-life-cycle-id='" + elemId + "']")[0];
}

function mountApp(domNode, renderCb, didUpdateCb, shouldUpdateCb) {
    return { domNode : domNode
           , onRender: renderCb
           , onDidUpdate : didUpdateCb
           , shouldUpdate : shouldUpdateCb
           };
}

function syncRedrawApp(app) {
    React.render(GhcjsAppFactory({ onRender: app.onRender
                                 , onDidUpdate: app.onDidUpdate
                                 , shouldUpdate: app.shouldUpdate
                                 }), app.domNode);
}

function attachRouteWatcher(routeChangeCb) {
    window.onhashchange = function() {
        routeChangeCb(location.hash);
    };
}

function setRoute(route) {
    // TODO (asayers): Setting location.hash is kinda slow, so ideally we'd
    // wait for a ReactJS animation frame before updating it.
    location.hash = route;
}

module.exports =
    { mkDomNode:          mkDomNode
    , queryLifeCycleNode: queryLifeCycleNode
    , mountApp:           mountApp
    , syncRedrawApp:      syncRedrawApp
    , attachRouteWatcher: attachRouteWatcher
    , setRoute:           setRoute
    };

// the global variable we're using in the bindings
h$reactjs = module.exports;

// TODO (meiersi): consider registering individual callbacks with ReactJS
// event handlers. Moreover, we'll need to use extensible retention once we
// want to memoize render-calls by introducing custom components.
// h$registerExtensibleRetention(scanTree);
