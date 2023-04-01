// https://riptutorial.com/svg/example/17731/dragging-svg-elements

// Makes an element in an SVG document draggable.
// Fires custom `dragstart`, `drag`, and `dragend` events on the
// element with the `detail` property of the event carrying XY
// coordinates for the location of the element.

var pos = {};
var new_point = {};

function makeDrawable(el){
  if (!el) return console.error('makeDrawable() needs an element');
  let svg = el;
  let pt=svg.createSVGPoint(), doc=svg.ownerDocument;
  // Convert mouse position from screen space to coordinates of el
  function inElementSpace(evt){
    pt.x=evt.clientX; pt.y=evt.clientY;
    return pt.matrixTransform(el.getScreenCTM().inverse());
  }

  while (svg && svg.tagName!='svg') svg=svg.parentNode;
  if (!svg) return console.error(el, 'must be an SVG wrapper');
  el.addEventListener("dblclick", (e) => {
    new_point = inElementSpace(e);
    let event = new Event("pointadd");
    el.dispatchEvent(event)
  });
}

function makeDraggable(el){
  if (!el) return console.error('makeDraggable() needs an element');
  let svg = el;
  while (svg && svg.tagName!='svg') svg=svg.parentNode;
  if (!svg) return console.error(el, 'must be inside an SVG wrapper');
  let pt=svg.createSVGPoint(), doc=svg.ownerDocument;

  let root = doc.rootElement || doc.body || svg;
  let xlate, txStartX, txStartY, mouseStart;
  let xforms = el.transform.baseVal;

  el.addEventListener('pointerdown', startMove, false);
  el.addEventListener("dblclick", (e) => {
    console.log("double Click on a Point.");
    fireEvent('pointremove');
  });

  function startMove(evt){
    // We listen for mousemove/up on the root-most
    // element in case the mouse is not over el.
    root.addEventListener('pointermove', handleMove, false);
    root.addEventListener(  'pointerup', finishMove, false);

    // Ensure that the first transform is a translate()
    xlate = xforms.numberOfItems>0 && xforms.getItem(0);
    if (!xlate || xlate.type != SVGTransform.SVG_TRANSFORM_TRANSLATE){
      xlate = xforms.createSVGTransformFromMatrix( svg.createSVGMatrix() );
      xforms.insertItemBefore( xlate, 0 );
    }
    txStartX=xlate.matrix.e;
    txStartY=xlate.matrix.f;
    mouseStart = inElementSpace(evt);
    fireEvent('dragstart');
  }

  function handleMove(evt){
    var point = inElementSpace(evt);
    xlate.setTranslate(
      txStartX + point.x - mouseStart.x,
      txStartY + point.y - mouseStart.y
    );
    fireEvent('drag');
  }

  function finishMove(evt){
    root.removeEventListener('pointermove', handleMove, false);
    root.removeEventListener(  'pointerup', finishMove, false);
    fireEvent('dragend');
  }

  function fireEvent(eventName){
    let event = new Event(eventName);
    event.detail = { x:xlate.matrix.e, y:xlate.matrix.f, id:el.id  };
    pos = { x:xlate.matrix.e, y:xlate.matrix.f, id:el.id };
    return el.dispatchEvent(event);
  }

  // Convert mouse position from screen space to coordinates of el
  function inElementSpace(evt){
    pt.x=evt.clientX; pt.y=evt.clientY;
    return pt.matrixTransform(el.parentNode.getScreenCTM().inverse());
  }
}



// WIP
// http://jsfiddle.net/robertc/kKuqH/
// https://codepen.io/ingvoo/pen/NNjWyN
function makeMovable(el){
  console.log("movable");
  function drag_start(event) {
    let style = window.getComputedStyle(event.target, null);
    event.dataTransfer.setData("text/plain",
                               (parseInt(style.getPropertyValue("left"),10) - event.clientX) + ',' + (parseInt(style.getPropertyValue("top"),10) - event.clientY));
  }
  function drag_over(event) {
    event.preventDefault();
    return false;
  }
  function drop(event) {
    let offset = event.dataTransfer.getData("text/plain").split(',');
    el.style.left = (event.clientX + parseInt(offset[0],10)) + 'px';
    el.style.top = (event.clientY + parseInt(offset[1],10)) + 'px';
    event.preventDefault();
    return false;
  }
  el.addEventListener('dragstart',drag_start,false);
  document.body.addEventListener('dragover',drag_over,false);
  document.body.addEventListener('drop',drop,false);
}
