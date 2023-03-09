import { css } from "uebersicht";
// import React from "react";
// import React, { useRef, useState, useMemo } from "react";
// import { Canvas, useFrame } from '@react-three/fiber';
import * as THREE from "three"

const options = {
  top: "15px",
  left: "20px",
  width: "20vw",
  height: "40vw",
};

export const refreshFrequency = 1000;

export const className = css({
  left: options.left,
  top: options.top,
  width: options.width,
  color: "white"
});

export const render = ( {output} ) => {
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.1, 1000 );

  const renderer = new THREE.WebGLRenderer();
  renderer.setSize( window.innerWidth, window.innerHeight );
  document.body.appendChild( renderer.domElement );

  const geometry = new THREE.BoxGeometry();
  const material = new THREE.MeshLambertMaterial( { color: 0x00ff00 } );
  const cube = new THREE.Mesh( geometry, material );
  scene.add( cube );

  camera.position.z = 5;

  const animate = function () {
    requestAnimationFrame( animate );

    cube.rotation.x += 0.01;
    cube.rotation.y += 0.01;

    renderer.render( scene, camera );
  };

  animate();
  
  return (
    // <></>
    <div></div>
  );
}

