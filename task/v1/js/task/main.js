"use strict"

// ----- Meta parameters (for testing) -----
let speed = "normal"; //fast, normal
let showNetworkWalk = false;
let showNavButtons = false;
let showFeedback = true;

// ----- Experiment Paramenters (CHANGE ME) -----
let networkSize = 10; //don't change without also updating network structure in networkFunctions.js
let nNetworkTrials = 200; //# of trials in illegal transition task
let breakEveryNTrials = 100;
let nPracticeTrials = 20;
let stimInterval = (speed == "fast") ? 50 : 2000; //1500 is default for now
let expStage = "main1-1"; //initialize expStage (make sure matches instructions)
let illegalProbability = 0.2; //frequency of illegal transitions
let correctTime = 1000;
let incorrectTime = 3000;
let practiceAccCutoff = 80;

// task variables
let taskNetwork = new Network(), activeNode, prevNode, transitionType;
let taskFunc, transitionFunc, stimTimeout, feedbackShown, missedSkip, falseAlarm;
let trialAttempt, consecutiveCorrectOnFirstTryTrials, oldParentDiv; //for drag task
let trialHistory = [];
let networkPrepared = false;
let actionArr, stimArr, switchRepeatArr, buffer, stimSet, stroopOnset, trialIsRepeat, trialIsNA, switchType, accArr;
let canvas, ctx, ntCanvas, ntCtx; //canvas variables
let data=[], taskName, trialCount, blockTrialCount, acc, accCount, stimOnset, respOnset, respTime, block, partResp, runStart, legalIllegalArray = [], trialType, taskSet; //variables for data logging
let breakOn = false, repeatNecessary = false; //variables for block breaks and repeating practie blocks
let sectionStart, sectionEnd, sectionType, sectionTimer; //for logging non experimental sections (instruction and break screens)
let dropOnset, prevDropOnset; // global variables for logging drag task drop events
let imageSize = 150, imageScale = 0.8;
let keyListener = 0;
/*  key press listener values:
      0: No key press expected/needed => do nothing. KL => 3
      1: Key press expected (triggered by stimulus appearing) KL => 2
      2: Key press from 1 received. Reset to 0 on keyup.  promptLetGo() if new trial starts without them having let go yet.
      3: Key press from 0 still being held down. Reset to 0 on keyup.  promptLetGo() if new trial starts without them having let go yet.
      4: Screen Size too small, "press any button to continue"
      5: Press button to start experiment (from instructions)
      6: Press button to continue to instructions (from feedback)
      7: Proceed to next block of task (from block break screen)
*/

function experimentFlow(){
  // set block and trial counts (unless repeat of task)
  trialCount = 1;
  blockTrialCount = 1;
  accCount = 0;
  if (!repeatNecessary) {
    block = 1;
  } else {
    block++;
  }

  // go to the relevant task based on expStage variable
  if (expStage.indexOf("main1") != -1){
    networkDragTask();
  } else if (expStage.indexOf("main2") != -1){
    practiceIllegalTransitionTask();
  } else if (expStage.indexOf("main3") != -1){
    illegalTransitionTask();
  } else if (expStage.indexOf("main4") != -1){
    oddOneOutTest();
  } else {
    endOfExperiment();
  }
}

// Experiment starts here
function startExperiment(){

  setUpCanvases();

  // create key press listener
  $("body").keypress(function(event){keyPressFunction(event)});

  // create key release listener
  $("body").keyup(function(event){keyUpFunction(event)});

  // having set up all the various key and button listeners, start task
  runStart = new Date().getTime();
  setUpNetwork();
  prepareNetworkDiagram();
  runInstructions();
  // experimentFlow()
}

function keyPressFunction(event){
  if (keyListener == 0) { //bad press
    keyListener = 3;
  } else if (keyListener == 1) { //good press
    keyListener = 2; //await key up

    // accuracy
    partResp = event.which;
    respOnset = new Date().getTime() - runStart;
    respTime = respOnset - stimOnset;
  }
}

function keyUpFunction(event){
  if (keyListener == 2 ) { //good press release
    if (taskName != "illegalTransitionTask") {
      clearTimeout(stimTimeout);
      transitionFunc();
    }
    keyListener = 0;
  } else if (keyListener == 3) { //resets bad press to 0
    keyListener = 0;
  } else if (keyListener == 4) { //screen size warning
    keyListener = 0;
    countDown(3, "fast");
  } else if (keyListener == 5) { //press button to start task (instructions)
    keyListener = 0;
    // log data
    sectionEnd = new Date().getTime() - runStart;
    logSectionData();
    // go to next experiment
    keyListener = 0;
    experimentFlow();
  } else if (keyListener == 6) { //navigates from task feedback to instructions (handles repeats)
    keyListener = 0;
    // log data
    sectionEnd = new Date().getTime() - runStart;
    logSectionData();
    // go to instructions
    navigateInstructionPath(repeatNecessary);
  } else if (keyListener == 7) { //block break screen
    keyListener = 0;
    clearInterval(sectionTimer);
    logSectionData();
    // increment block
    block++;
    blockTrialCount = 1;

    // log data
    logSectionData();

    // resume task
    keyListener = 0; sectionType = "mainTask";
    countDown(taskFunc, 3);
  }
}
