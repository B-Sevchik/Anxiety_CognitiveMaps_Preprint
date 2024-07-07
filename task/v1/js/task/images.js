// -------------------------------------------//
//   Selected Images to use in experiment
// -------------------------------------------//

let instructionImages = {
  1: '././images/handsOnKeyboard5.png',
  2: '././images/handsOnKeyboard2.png',
  3: '././images/networkExample.png',
  4: '././images/handsOnKeyboard6.png',
  5: '././images/stroopInstructions.png'
}

let n_neutral_images_in_bank = 10
let n_neutral_images_in_network = 6
let selected_neutral_SRCs = getImageSRCs(n_neutral_images_in_bank, n_neutral_images_in_network, 'neutral_images')

let n_threat_images_in_bank = 4
let n_threat_images_in_network = 4
let selected_threat_SRCs = getImageSRCs(n_threat_images_in_bank, n_threat_images_in_network, 'threat_images')

// load images and store in selectedImages var
let selected_threat_images = convertToImages(selected_threat_SRCs)
let selected_neutral_images = convertToImages(selected_neutral_SRCs)


let all_selected_images_unshuffled = selected_neutral_images.concat(selected_threat_images);
let selectedImages = _.shuffle(all_selected_images_unshuffled)

// -------------------------------------------//
//  Code for displaying images to participants
// -------------------------------------------//

function createImageTable(){
  // defines how many images fit per row (within 900px width and 26px padding)
  let chunkSize = 5;

  // create image table
  let imageTable = document.createElement("div");
  imageTable.className = "imageTable";

  // add images to imageTable
  selectedImages.forEach((imageObj, i) => {

    // create div element to hold image
    let imageDiv = document.createElement("div");
    imageDiv.className = "imageDiv";

    // transfor imageObj to new variable for resizing
    let newImageObj = new Image();
    newImageObj.src = imageObj.src;
    newImageObj.ondragstart = function(){return false;};

    // resize image
    newImageObj.width = (900 / chunkSize) - 26; //

    // add image to imageDiv
    imageDiv.appendChild(newImageObj);

    // add imageDiv to imageRow
    imageTable.appendChild(imageDiv);
  });

  // wrap imageTable in div wrapper of class "insertedContent"
  let imageTableDiv = document.createElement("div");
  imageTableDiv.className = "insertedContent";
  imageTableDiv.appendChild(imageTable);
}

function prepareNetworkDiagram(){
  document.getElementById("network-diagram").style.display = "flex";
  createSVG("svg1","#network-container-sm", 450*imageScale + 'px', 850*imageScale + 'px');
  drawSVGLines("svg1", "plot", "#network-container-sm");
  insertImagesIntoNetworkDiagram()
  getNetworkDiagramReady();
  $("#network-diagram").hide()
}

function insertImagesIntoNetworkDiagram(){
  // fill images into network diagram
  for (let i = 0; i < taskNetwork.nodes.length; i++) {
    let imageDiv = new Image;
    imageDiv.src = taskNetwork.nodes[i].img.src
    imageDiv.width = 100 * imageScale; //
    imageDiv.id = "img" + i;
    document.getElementById("plot"+i).append(imageDiv);
  }
}

function createSVG(id, location, h, w, before = true){
  // add svg (for drawing lines)
  let svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute('id', id);
  svg.setAttribute('width', w);
  svg.setAttribute('height', h);
  svg.style.position = 'absolute';
  svg.setAttribute('z-index', '-1');
  if (before) {
    $(svg).insertBefore(location);
  } else {
    $(svg).insertAfter(location);
  }
}

function drawSVGLines(svg, id_tag, reference){
  // loop through html elements and draw lines for each
  document.body.querySelectorAll("*").forEach(node => {
    if (node.id.indexOf(id_tag) != -1) {
      let x1 = $("#"+node.id).offset().left+ (node.offsetWidth / 2) - $(reference).offset().left;
      let y1 = $("#"+node.id).offset().top + (node.offsetHeight / 2) - $(reference).offset().top;
      let nodeN = node.id.match(/\d+/g);
      let neighbors = nodeNeighbors[nodeN];
      neighbors.forEach( neighbor => {
        let name = "#"+id_tag + neighbor;
        let x2 = $(name).offset().left+ $(name).outerWidth() / 2 - $(reference).offset().left;
        let y2 = $(name).offset().top + $(name).outerHeight() / 2 - $(reference).offset().top;

        // draw line
        drawLine(x1, y1, x2, y2, svg)
      });
    }
  });
}

function clearSVGArrows(svg){
  document.getElementById(svg).childNodes.forEach(node => {
    if (node.tagName == 'line') {
      node.remove();
    }
  })
}

function drawSVGArrow(first_n, second_n, reference, svg){
  // grab reference to boxes
  let node1 = document.getElementById("plot"+first_n)
  let node2 = document.getElementById("plot"+second_n)

  // get x,y coordinate of both locations (top left corner of box)
  let node1_x = $("#plot"+first_n).offset().left - $(reference).offset().left;
  let node2_x = $("#plot"+second_n).offset().left - $(reference).offset().left;
  let node1_y = $("#plot"+first_n).offset().top - $(reference).offset().top;
  let node2_y = $("#plot"+second_n).offset().top - $(reference).offset().top;

  // get center of those locations by adding half of width and height
  let x1 = node1_x + (node1.offsetWidth / 2);
  let y1 = node1_y + (node1.offsetHeight / 2);
  let x2 = node2_x + (node2.offsetWidth / 2);
  let y2 = node2_y + (node2.offsetHeight / 2);
  // console.log("#plot"+first_n + " coords (x,y)");
  // console.log(x1, y1);
  // console.log("#plot"+second_n + " coords (x,y)");
  // console.log(x2, y2);

  // draw buffers around box (fo visualization only)
  let firstBuffer = 5, secondBuffer = 25;
  // drawRect(node1_x - firstBuffer, node1_y - firstBuffer, node1.offsetWidth + firstBuffer*2, node1.offsetHeight+firstBuffer*2, svg);
  // drawRect(node2_x - secondBuffer, node2_y - secondBuffer, node2.offsetWidth+secondBuffer*2, node2.offsetHeight+secondBuffer*2, svg);

  // arrows shouldn't go over buffer, so need to adjust x and y values
  let w_adj, h_adj;
  let w = Math.abs(x2 - x1);
  let h = Math.abs(y2 - y1);
  // console.log("width of t: " + w);
  // console.log("height of t: " + h);

  if (h < w) {
    w_adj = node1.offsetWidth/2;
    h_adj = (h / w) * w_adj
  } else {
    h_adj = node1.offsetHeight/2;
    w_adj = (w / h) * h_adj
  }
  // console.log(w_adj);
  // console.log(h_adj);

  let adj_x1, adj_x2, adj_y1, adj_y2;
  // adjust x values
  if (x2 > x1) {
    adj_x1 = x1 + w_adj + firstBuffer
    adj_x2 = x2 - w_adj - secondBuffer
  } else {
    adj_x1 = x1 - w_adj - firstBuffer
    adj_x2 = x2 + w_adj + secondBuffer
  }

  // adjust y values
  if (y2 > y1) {
    adj_y1 = y1 + h_adj
    adj_y2 = y2 - h_adj
  } else {
    adj_y1 = y1 - h_adj
    adj_y2 = y2 + h_adj
  }

  // create arrow head marker
  let defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
  let marker = document.createElementNS('http://www.w3.org/2000/svg', 'marker');
  marker.setAttribute('id', 'Triangle');
  marker.setAttribute('viewBox', '0 0 10 10');
  marker.setAttribute('refX', '0');
  marker.setAttribute('refY', '5');
  marker.setAttribute('markerUnits', 'strokeWidth');
  marker.setAttribute('markerWidth', '3');
  marker.setAttribute('markerHeight', '3');
  marker.setAttribute('fill', 'red');
  marker.setAttribute('orient', 'auto');
  let path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  marker.appendChild(path);
  path.setAttribute('d', 'M 0 0 L 10 5 L 0 10 z');
  document.getElementById(svg).appendChild(defs);
  defs.appendChild(marker);

  // draw line
  drawLine(adj_x1, adj_y1, adj_x2, adj_y2, svg,'red', '10px', arrow=true)
}

function drawLine(x1,y1,x2,y2,svg,color='black',thickness='5px', arrow=false){
  let newLine = document.createElementNS('http://www.w3.org/2000/svg','line');
  newLine.setAttribute('x1',x1);
  newLine.setAttribute('y1',y1);
  newLine.setAttribute('x2',x2);
  newLine.setAttribute('y2',y2);
  newLine.setAttribute("stroke", color);
  newLine.setAttribute("stroke-width", thickness);
  if (arrow) {
    newLine.setAttribute('marker-end', 'url(#Triangle)');
  }
  $("#"+svg).append(newLine);
}

function drawRect(x, y, w, h, svg){
  let rect = document.createElementNS('http://www.w3.org/2000/svg','rect');
  rect.setAttribute('x', x);
  rect.setAttribute('y',y);
  rect.setAttribute('width', w);
  rect.setAttribute('height',h);
  rect.setAttribute('fill',"none");
  rect.setAttribute("stroke", "black");
  rect.setAttribute("stroke-width", "1px");
  $("#"+svg).append(rect);
}

function getImageSRCs(images_in_bank_num, images_in_network_num, image_path){
  let src_arr = []
  for (var i = 1; i <= images_in_bank_num; i++) {
    src_arr.push(`././images/${image_path}/picture${i}.jpg`)
  }
  return _.sample(src_arr, images_in_network_num)
}

function convertToImages(selected_images_SRCs){
  let converted_images = new Array(selected_images_SRCs.length);
  for (var i = 0; i < selected_images_SRCs.length; i++) {
    converted_images[i] = new Image();
    converted_images[i].src = selected_images_SRCs[i]
  }
  return converted_images
}
