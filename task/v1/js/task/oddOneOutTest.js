let nodeSetIterator;
function oddOneOutTest() {
  document.body.style.cursor = 'auto';
  // holds task node sets
  let taskNodeSets = [];

  // fill taskNodesSets
  createNodeSetArr();
  // iterationTesting();

  // run algorithm to set up main task array
  // prevents repeat of nodes from one trial to next
  let mainTaskArr;
  let algorithmIterationCap = 10000;
  for (let i = 1; i < algorithmIterationCap; i++) {
    mainTaskArr = shuffleNodeSets();
    if (mainTaskArr.length != 0) {
      break;
    }
  }
  console.log(mainTaskArr);
  // set section type
  sectionType = "mainTask";
  taskName = "oddOneOutTest";

  // show task div and hide instructions
  $('#instructionsDiv').hide();
  $("#navButtons").hide();
  $("#oddOneOutTaskDiv").show();

  // set up task vars
  nodeSetIterator = 0;
  let currentNodeSet = mainTaskArr[nodeSetIterator];
  let nodeSetDisplayStart;
  let pressedSameButtonCounter = 0, prevResponse;
  let earlyButtonPressCounter = 0, spamEvent = false; spamEventCounter = 0;

  // set up DOM elements variables
  let image1 = document.getElementById("OO_image1");
  let image2 = document.getElementById("OO_image2");
  let image3 = document.getElementById("OO_image3");

  // set listener function of image clicks
  $("#OO_image1").click(function(){
    processImageClick(1);
  })

  $("#OO_image2").click(function(){
    processImageClick(2);
  })

  $("#OO_image3").click(function(){
    processImageClick(3);
  })

  // start by displaying first node set
  displayNodeSet(currentNodeSet);

  // FUNCTIONS:
  function processImageClick(imageNum){
    respOnset = new Date().getTime() - runStart;
    if (respOnset - stimOnset > 300) {
        acc = (currentNodeSet.indexer[imageNum - 1] == 1) ? 1 : 0;
        partResp = imageNum;
        respOnset = new Date().getTime() - runStart;
        respTime = respOnset - stimOnset;

        // log data
        logOddOneOutData(currentNodeSet, imageNum);


        if (nodeSetIterator < mainTaskArr.length - 1) {
          prevResponse = imageNum;
          nodeSetIterator++;
          currentNodeSet = mainTaskArr[nodeSetIterator];
          image1.src = 'images/blank.png';
          image2.src = 'images/blank.png';
          image3.src = 'images/blank.png';
          setTimeout(function(){
            displayNodeSet(currentNodeSet);
          }, 250);
        } else {
          // end of experiment, proceed to next instructions
          $("#oddOneOutTaskDiv").hide();
          navigateInstructionPath();
          // endOfExperiment();
        }
      // }
    }
  }

  function displayNodeSet(nodeSet){
    stimOnset = new Date().getTime() - runStart;

    // display images in node set
    image1.src = nodeSet.nodes[0].img.src;
    image2.src = nodeSet.nodes[1].img.src;
    image3.src = nodeSet.nodes[2].img.src;
  }

  function shuffleNodeSets(){
    // shuffling function for oddoneout task
    // shuffle node sets so there are no repeats in target images
    let newArr = [];
    let baseArr = shuffle($.extend(true,[],taskNodeSets)); //copy taskNodeSets into new variable (shuffled)

    // randomly splice from baseArr into newArr to start algorithm
    newArr.push(baseArr.splice(Math.floor(Math.random()*baseArr.length),1)[0]);

    // build array one nodeset at a time
    while (baseArr.length > 0) {
      let nextNode = getNextNodeSet();
      if (nextNode.length != 0) {
        newArr.push(nextNode);
      } else {
        // failure mode, return nothing
        return [];
      }
    }

    // if while loop succeeds, return newArr
    return newArr;

    function getNextNodeSet(){
      // identify last node set in newArr
      let lastNodeSet = newArr[newArr.length - 1];

      // filter baseArr to include only nodesets that don't have the same target node as lastNodeSet
      let filteredArr = baseArr.filter(checkNodeSet);

      function checkNodeSet(nodeSet){
        // // filter to those nodesets that don't include the previous target node, and whose images are all different than previous (at any location)
        let targetNode = lastNodeSet.nodes[lastNodeSet.indexer.indexOf(1)];
        return !nodeSet.nodes.includes(targetNode) && nodeSet.nodes.every((n, i) => n != lastNodeSet.nodes[i]);


        //filter out all nodeSets that share nodes with previous nodeset
        // return !nodeSet.nodes.some(n => lastNodeSet.nodes.includes(n));
      }

      if (filteredArr.length == 0) {

        // console.log("Oops, no values left in filteredArr");
        // console.log("lastNodeSet: ", lastNodeSet);
        // console.log("baseArr: ", baseArr);
        return [];

      } else {

        // randomly sample next node from filteredArr
        let nextNodeSet = _.sample(filteredArr,1)[0];

        // splice nextNodeSet and return value
        return baseArr.splice(baseArr.indexOf(nextNodeSet),1)[0];
      }
    }
  }

  function iterationTesting(){
    // for testing algorithmic efficiency
    // logs how many iterations algorithm takes to find solution
    let nTests = 100;
    let algorithmicCap = 1000; //after testing 100,000 iterations of algorithm, max was 148 attempts to find solution
    let iterations = {
      "No Solution": 0
    };
    for (var j = 1; j < nTests; j++) {
      console.log("Test#: ", j);
      let iterationCount = getIterationCount();
      if (iterationCount in iterations) {
        iterations[iterationCount]++;
      } else {
        iterations[iterationCount] = 1;
      }
      console.log("Test took ", iterationCount, " steps.");
    }
    console.log(iterations);

    function getIterationCount(){
      createNodeSetArr();
      for (var i = 1; i < algorithmicCap; i++) {
        taskArr = shuffleNodeSets();
        if (taskArr.length != 0) {
          return i;
        }
      }
      return `No Solution (${algorithmicCap} attempts)`;
    }
  }

  function createNodeSetArr(){
    // create node sets for task
    taskNetwork.nodes.forEach((node) => {

      // only use internal nodes
      if (!node.isBoundaryNode) {

        let nodeSetsBatch = [];

        // choose two nodes from other community
        let oppCommNumber = (node.communityNumber == 1) ? 2 : 1;
        let oppCommNodes = taskNetwork.communityNodes(oppCommNumber);
        let oppCommSelection = _.sample(oppCommNodes.filter(n => !n.isBoundaryNode),2);

        //  do 6 times, all permutations
        let count = 1;
        for (let i = 0; i < 3; i++) { //three times per object

          for (let j = 0; j < 2; j++) { //twice per others (6 total)

            // console.log(count);
            // console.log(node.name);
            count++;

            let newNodeSet = {
              nodes: [],
              indexer: [],
              target: node.name
            }

            let nodeSetArr;
            let oddOneIndexer = [0,0];

            // starting array
            if (j == 0) {
              nodeSetArr = [...oppCommSelection];
            } else {
              nodeSetArr = [...oppCommSelection].reverse();
            }

            // splice in target
            nodeSetArr.splice(i, 0, node);
            oddOneIndexer.splice(i, 0, 1);

            // update newNodeSet
            newNodeSet.nodes = nodeSetArr;
            newNodeSet.indexer = oddOneIndexer;

            // push newNodeSet into nodeSetsArr
            nodeSetsBatch.push(newNodeSet);
          }
        }
        // console.log(nodeSetsBatch);
        // add node set to task set
        taskNodeSets = taskNodeSets.concat(nodeSetsBatch);

      }

    });
  }
}

// OLD VERSION
// function createNodeSetArr(){
//   // create node sets for task
//   taskNetwork.nodes.forEach((node) => {
//
//     if (!node.isBoundaryNode) {
//
//       let nodeSetsBatch = [];
//       for (let i = 0; i < 3; i++) { //do three times per node
//         // object for node set
//         let newNodeSet = {
//           nodes: [],
//           indexer: []
//         }
//
//         // select two random neighbors from the other community
//         let oppCommNumber = (node.communityNumber == 1) ? 2 : 1;
//         let oppCommNodes = taskNetwork.communityNodes(oppCommNumber);
//
//         let oppCommSelection;
//         do {
//           oppCommSelection = _.sample(oppCommNodes.filter(n => !n.isBoundaryNode),2);
//         } while (repeatedNodeSelection()); //check nodeSetsBatch
//
//         function repeatedNodeSelection(){
//           return (nodeSetsBatch.length == 0) ? false : nodeSetsBatch.some(ns => oppCommSelection.every(n => ns.nodes.includes(n)));
//         }
//
//         // create nodeSet node array and indexer (identify which node is oddOneOut)
//         let nodeSetArr = oppCommSelection;
//         nodeSetArr.splice(i, 0, node);
//         let oddOneIndexer = [0,0];
//         oddOneIndexer.splice(i, 0, 1);
//
//         // update newNodeSet
//         newNodeSet.nodes = nodeSetArr;
//         newNodeSet.indexer = oddOneIndexer;
//
//         // push newNodeSet into nodeSetsArr
//         nodeSetsBatch.push(newNodeSet);
//       }
//
//       // add node set to task set
//       taskNodeSets = taskNodeSets.concat(nodeSetsBatch);
//
//     }
//
//   });
// }
