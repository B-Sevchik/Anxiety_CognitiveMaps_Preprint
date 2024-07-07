class Network {
  constructor(){
    this.nodes = [];
    this.nCommunities = 2;
  }

  communityNodes(communityNumber){
    let nodeArr  = [];
    this.nodes.forEach((node) => {
      if (node.communityNumber == communityNumber) {
        nodeArr.push(node);
      }
    })
    return nodeArr;
  }

  addNode(newNode){
    this.nodes.push(newNode);
  }
}

class Node {
  constructor(i, imageJpg, isThreat) {
    this.name = `Node${i}`;
    this.index = i;
    this.img = imageJpg;
    this.neighbors = [];
    this.coord = {x: NaN, y: NaN};
    this.rad = 7;
    this.color = "black";
    this.visitCount = 0;
    this.community = NaN;
    this.communityNumber = NaN;
    this.associatedWithTask = false;
    this.isBoundaryNode = false;
    this.threat = isThreat;
  }

  addNeighbor(newNeighbor) {
    // add node as neighbor if it doesn't already exist
    if (this.neighbors.indexOf(newNeighbor != -1)) {
      this.neighbors.push(newNeighbor);
    }
  }

  activate(){
    activeNode.rad = 15;
    activeNode.color = "red";
    this.visitCount++;
  }

  reset(){
    this.rad = 7;
    this.color = "black";
  }
}

let nodeNeighbors = {
  0: [1,2,3,4],
  1: [0,2,3,4],
  2: [0,1,3,4],
  3: [0,1,2,4],
  4: [0,1,2,3,5],
  5: [4,6,7,8,9],
  6: [5,7,8,9],
  7: [5,6,8,9],
  8: [5,6,7,9],
  9: [5,6,7,8],
}

function setUpNetwork(){
  // define coordinates for nodes for drawing purposes
  let coordinates = {
    1: {x: (1/5), y: (4/5)}, 2: {x: (1/20), y: (1/2)},
    3: {x: (1/5), y: (1/5)}, 4: {x: (17/40), y: (1/3)},
    5: {x: (17/40), y: (2/3)}, 6: {x: (23/40), y: (1/3)},
    7: {x: (23/40), y: (2/3)}, 8: {x: (4/5), y: (1/5)},
    9: {x: (19/20), y: (1/2)}, 10: {x: (4/5), y: (4/5)}
  }

  // defines which nodes are being associated and whose associations will need to be inferred
  let associationStatuses = {
    1: false, 2: true, 3: true, 4: true, 5: true,
    6: true, 7: true, 8: false, 9: true, 10: true
  }

  // defines if node is a boundary node (connecting to other community)
  let boundaryNodes = {
    1: false, 2: false, 3: false, 4: false, 5: true,
    6: true, 7: false, 8: false, 9: false, 10: false
  }

  // create network with nodes for each image
  // for (var i = 0; i < selectedImages.length; i++) {
  //   taskNetwork.addNode(new Node(i + 1, selectedImages[i], selectedImages_png[i]));
  // }
  // for (var i = 0; i < selected_threat_images.length; i++) {
  //   taskNetwork.addNode(new Node(i + 1, selected_threat_images[i]))
  // }
  // for (var i = 0; i < selected_neutral_images.length; i++) {
  //   taskNetwork.addNode(new Node(i + 1, selected_neutral_images[i]))
  // }
  let imagePairings = {}

  // left half of network (two are threat)
  shuffled_indices = shuffle([1,2,3,4])
  imagePairings[shuffled_indices[0]] = "threat"
  imagePairings[shuffled_indices[1]] = "threat"
  imagePairings[shuffled_indices[2]] = "neutral"
  imagePairings[shuffled_indices[3]] = "neutral"

  // right half of network (two are threat)
  shuffled_indices = shuffle([7, 8, 9, 10])
  imagePairings[shuffled_indices[0]] = "threat"
  imagePairings[shuffled_indices[1]] = "threat"
  imagePairings[shuffled_indices[2]] = "neutral"
  imagePairings[shuffled_indices[3]] = "neutral"

  // boundary nodes, both neutral
  imagePairings[5] = "neutral"
  imagePairings[6] = "neutral"

  //loop from 1-10
  let threatIterator = 0;
  let neutralIterator = 0;
  for (var i = 1; i < 11; i++) {
    if (imagePairings[i] == 'threat') {
      taskNetwork.addNode(new Node(i, selected_threat_images[threatIterator], true))
      threatIterator++
    }
    else {
      taskNetwork.addNode(new Node(i, selected_neutral_images[neutralIterator], false))
      neutralIterator++
    }
  }

  console.log(taskNetwork)

  // add neighbors to objects as specified in nodeNeighbors var
  taskNetwork.nodes.forEach((node, i) => {
    nodeNeighbors[node.index-1].forEach((neighbor) => {
      taskNetwork.nodes[i].addNeighbor(taskNetwork.nodes[neighbor]);
    })
  })

  // add address to each node as specified in coordinates var
  taskNetwork.nodes.forEach((node, i) => {
    node.coord.x = coordinates[node.index].x * canvas.width;
    node.coord.y = coordinates[node.index].y * canvas.height;
  })

  // assign communities to nodes
  taskNetwork.nodes.forEach((node, i) => {
    if (i < 5) {
      node.community = "congruent";
      node.communityNumber = 1;
    } else if (i >= 5) {
      node.community = "incongruent";
      node.communityNumber = 2;
    } else {
      node.community = NaN;
      node.communityNumber = 0;
    }
  })

  //set association statuses and boundary status
  taskNetwork.nodes.forEach((node, i) => {
    node.associatedWithTask = associationStatuses[node.index];
    node.isBoundaryNode = boundaryNodes[node.index];
  })
}

function displayImage(){
  // clear canvas
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  // display image
  ctx.drawImage(activeNode.img,canvas.width/2 - activeNode.img.width/2,canvas.height/2-activeNode.img.height/2);
}

function drawNetwork(){
  $(".canvasas").show();
  $("#networkCanvas").show();

  // clear canvas
  ntCtx.clearRect(0, 0, ntCanvas.width, ntCanvas.height);

  // draw edges
  taskNetwork.nodes.forEach((node) => {
    node.neighbors.forEach((neighbor) => {
      ntCtx.beginPath();
      ntCtx.moveTo(node.coord.x,node.coord.y);
      ntCtx.lineTo(neighbor.coord.x,neighbor.coord.y);
      ntCtx.stroke();
    });
  });

  // draw nodes
  taskNetwork.nodes.forEach((node) => {
    ntCtx.fillStyle = node.color;
    ntCtx.beginPath();
    ntCtx.arc(node.coord.x,node.coord.y,node.rad,0,2*Math.PI);
    ntCtx.fill();
    // ntCtx.drawImage(node.img_png, node.coord.x - 75,node.coord.y - 75, 150, 150)
  });
}

function isCommunityTransition(){
  if (prevNode !== undefined) {
    return activeNode.communityNumber != prevNode.communityNumber;
  } else {
    return false;
  }
}

function transitionThreatKind(){
  if (!prevNode) {return NaN}

  let prevNodeType = (prevNode.threat) ? "threat" : "neutral"
  let currNodeType = (activeNode.threat) ? "threat" : "neutral"
  return prevNodeType + "-" + currNodeType
}

function getNetworkDiagramReady(){
  let nd = document.getElementById("network-diagram");
  //insert diagram back into main html (had been in instructions)
  $(nd).insertAfter("#networkDragTask");
  // center network diagram in middle of screen
  nd.style.position = "absolute";
  nd.style.top = "50%";
  nd.style.left = "50%";
  nd.style.transform = "translate(-50%, -50%)";

  // get svg ready (for drawing arrows of mistakes)
  createSVG("svg2","#network-container-sm", 450*imageScale + 'px', 800*imageScale + 'px', false);
  $("<h3 id='upperText' class='illegalText'></p>").insertBefore("#network-container-sm");
  document.getElementById("upperText").style.top = -150*imageScale + 'px';
  $("<h3 id='lowerText' class='illegalText'></p>").insertAfter("#network-container-sm");
  document.getElementById("lowerText").style.top = 425*imageScale + 'px';
}

function showNetworkTransition(legal=True){
  feedbackShown = true;
  let waitTime = (acc) ? correctTime : incorrectTime;

  // hide canvas and show feedback (with arrow for illegal transitions)
  $(".canvasas").hide();
  $("#network-diagram").show();
  clearSVGArrows("svg2");
  if (!legal) {
    drawSVGArrow(prevNode.index-1,activeNode.index-1,"#network-container-sm","svg2");
  }

  // draw text based on response (correct, missed skip, false alarm)
  if (acc) {
    $("#upperText").css("color", "green");
    document.getElementById("upperText").innerHTML = "Correct";
    document.getElementById("lowerText").innerHTML = "";
  } else if (missedSkip) {
    $("#upperText").css("color", "red");
    document.getElementById("upperText").innerHTML = "Incorrect";
    document.getElementById("lowerText").innerHTML = "Jill cheated! The task will resume in 3 seconds.";
  } else if (falseAlarm) {
    $("#upperText").css("color", "red");
    document.getElementById("upperText").innerHTML = "Incorrect";
    document.getElementById("lowerText").innerHTML = "Jill didn't cheat! The task will resume in 3 seconds.";
  }

  // proceed after delay
  setTimeout(function(){
    $(".canvasas").show();
    $("#network-diagram").hide()
    transitionFunc();
  },waitTime);
}
