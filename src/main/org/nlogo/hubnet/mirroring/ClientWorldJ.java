// (C) 2012 Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.hubnet.mirroring;

import org.nlogo.api.Perspective;
import org.nlogo.api.PerspectiveJ;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import static org.nlogo.hubnet.mirroring.ClientWorldS.TurtleKey;
import static org.nlogo.hubnet.mirroring.ClientWorldS.TurtleKeyComparator;
import static org.nlogo.hubnet.mirroring.ClientWorldS.LinkKey;
import static org.nlogo.hubnet.mirroring.ClientWorldS.LinkKeyComparator;

public abstract strictfp class ClientWorldJ
    implements org.nlogo.api.World {

  /**
   * whether or not to print messages for protocol errors.
   * false is useful when unit testing.
   */
  private final boolean printErrors;

  // these are package protected for unit testing...

  // since we want to keep the turtles sorted, but we also
  // want to be able to look them up just by who number, we
  // keep two parallel maps, one sorted, one not -- AZS 11/16/04

  java.util.SortedMap<TurtleKey, TurtleData> sortedTurtles;
  Map<Long, TurtleKey> turtleKeys;

  Map<Long, TurtleData> uninitializedTurtles = new HashMap<Long, TurtleData>();

  java.util.SortedMap<LinkKey, LinkData> sortedLinks;
  Map<Long, LinkKey> linkKeys;

  Map<Long, LinkData> uninitializedLinks = new HashMap<Long, LinkData>();

  public PatchData[] patchData;
  private int[] patchColors;

  public int[] patchColors() {
    return patchColors;
  }

  public ClientWorldJ(boolean printErrors, scala.Option<Integer> numPatches) {
    this.printErrors = printErrors;
    sortedTurtles = new TreeMap<TurtleKey, TurtleData>(new TurtleKeyComparator());
    turtleKeys = new HashMap<Long, TurtleKey>();
    sortedLinks = new TreeMap<LinkKey, LinkData>(new LinkKeyComparator());
    linkKeys = new HashMap<Long, LinkKey>();
    if (numPatches.isDefined()) {
      createPatches(numPatches.get().intValue());
    }
  }

  // temporary hack for the review tab experiments
  public void reset(){
    sortedTurtles = new TreeMap<TurtleKey, TurtleData>(new TurtleKeyComparator());
    turtleKeys = new HashMap<Long, TurtleKey>();
    sortedLinks = new TreeMap<LinkKey, LinkData>(new LinkKeyComparator());
    linkKeys = new HashMap<Long, LinkKey>();
  }

  protected void createPatches(int numPatches) {
    patchData = new PatchData[numPatches];
    patchColors = new int[numPatches];
    for (int i = 0; i < patchData.length; i++) {
      patchData[i] = new PatchData(i, (short) PatchData.COMPLETE(), 0, 0, 0.0, "", 0.0);
      patchData[i].patchColors_$eq(patchColors);
    }
  }

  protected org.nlogo.api.TrailDrawerInterface trailDrawer;

  public void setTrailDrawer(org.nlogo.api.TrailDrawerInterface trailDrawer) {
    this.trailDrawer = trailDrawer;
  }

  /**
   * Returns descriptions of the turtles in this world.
   * In the correct order for drawing.
   */
  public Iterable<TurtleData> getTurtles() {
    return sortedTurtles.values();
  }

  /**
   * Returns descriptions of the turtles in this world.
   * In the correct order for drawing.
   */
  public Iterable<LinkData> getLinks() {
    return sortedLinks.values();
  }

  /**
   * Returns descriptions of the patches in this world.
   */
  public PatchData[] getPatches() {
    return patchData;
  }

  public abstract int minPxcor();
  public abstract int maxPxcor();
  public abstract int minPycor();
  public abstract int maxPycor();

  public int worldWidth() {
    return maxPxcor() - minPxcor() + 1;
  }

  public int worldHeight() {
    return maxPycor() - minPycor() + 1;
  }

  private double patchSize = 13;

  public void patchSize(double patchSize) {
    this.patchSize = patchSize;
  }

  public double patchSize() {
    return perspectiveMode == PerspectiveMode.SERVER ? patchSize : ((StrictMath.max(viewWidth, viewHeight)) / ((radius * 2) + 1));
  }

  public double zoom() {
    return patchSize() / patchSize;
  }

  // for now we're not keeping track of this on the client,
  // but we could ev 4/24/08
  public boolean patchesAllBlack() {
    return false;
  }

  void updatePatch(PatchData patch) {
    if (patch.id() >= patchData.length) {
      handleError("ERROR: received update for "
          + "non-existent patch (" + patch.stringRep() + ").");
      return;
    }

    // otherwise, we'll need our version, if we've got one.
    PatchData bufPatch = patchData[(int) patch.id()];

    // if we haven't got one, this patch better have all its info...
    if (bufPatch == null && !patch.isComplete()) {
      handleError
          ("ERROR: received incremental update for non-existent patch (" + patch.stringRep() + ").");
    }

    // otherwise, perform the update...
    bufPatch.updateFrom(patch);
    patchColors[(int) patch.id()] = org.nlogo.api.Color.getARGBIntByRGBAList(bufPatch.pcolor());
  }

  void updateTurtle(TurtleData turtle) {
    Long simpleKey = Long.valueOf(turtle.id());
    TurtleKey sortedKey = turtleKeys.get(simpleKey);

    // if this turtle has died, just remove it...
    if (turtle.isDead()) {
      if (sortedKey == null) {
        handleError("ERROR: received death message for "
            + "non-existent turtle (" + turtle.stringRep() + ").");
        return;
      }
      sortedTurtles.remove(sortedKey);
      turtleKeys.remove(simpleKey);
      return;
    }

    // otherwise, we'll need our version, if we've got one.
    TurtleData bufTurtle = null;

    if (sortedKey == null) {
      bufTurtle = uninitializedTurtles.get(simpleKey);
      if (bufTurtle != null) {
        sortedKey = new TurtleKey(turtle.id(), turtle.getBreedIndex());
        sortedTurtles.put(sortedKey, bufTurtle);
        turtleKeys.put(simpleKey, sortedKey);
        uninitializedTurtles.remove(simpleKey);
      }
    }

    if (sortedKey != null) {
      bufTurtle = sortedTurtles.get(sortedKey);
    }

    // if we haven't got one, this turtle better have all its info...
    if (bufTurtle == null) {
      if (turtle.isComplete()) {
        sortedKey = new TurtleKey(turtle.id(), turtle.getBreedIndex());
        sortedTurtles.put(sortedKey, turtle);
        turtleKeys.put(simpleKey, sortedKey);
      } else {
        handleError("ERROR: received incremental update for non-existent turtle ("
            + turtle.stringRep() + ").");
      }
      return;
    }

    // otherwise, perform the update...
    bufTurtle.updateFrom(turtle);

    if (bufTurtle.getBreedIndex() != sortedKey.breedIndex()) {
      // the breed of this turtle changed so we need to make
      // a new key in the sortedTurtles map ev 5/19/08
      sortedTurtles.remove(sortedKey);
      sortedKey = new TurtleKey(bufTurtle.id(), turtle.getBreedIndex());
      sortedTurtles.put(sortedKey, bufTurtle);
      turtleKeys.put(simpleKey, sortedKey);
    }
  }

  void updateLink(LinkData link) {
    Long simpleKey = Long.valueOf(link.id());
    LinkKey sortedKey = linkKeys.get(simpleKey);

    if (link.isDead()) {
      if (sortedKey == null) {
        handleError
            ("ERROR: received death message for non-existent link ( "
                + link.stringRep() + " ).");
        return;
      }

      sortedLinks.remove(sortedKey);
      linkKeys.remove(simpleKey);

      return;
    }

    LinkData bufLink = null;

    if (sortedKey == null) {
      bufLink = uninitializedLinks.get(simpleKey);
      if (bufLink != null) {
        sortedKey = new LinkKey(link.id(), link.end1Id(), link.end2Id(), link.getBreedIndex());
        linkKeys.put(simpleKey, sortedKey);
        sortedLinks.put(sortedKey, link);
        uninitializedLinks.remove(simpleKey);
      }
    }

    if (sortedKey != null) {
      bufLink = sortedLinks.get(sortedKey);
    }

    // if we haven't got one, this link better have all its info...
    if (bufLink == null) {
      if (link.isComplete()) {
        sortedKey = new LinkKey(link.id(), link.end1Id(), link.end2Id(), link.getBreedIndex());
        linkKeys.put(simpleKey, sortedKey);
        sortedLinks.put(sortedKey, link);
      } else {
        handleError("ERROR: received incremental update for "
            + "non-existent turtle (" + link.stringRep() + ").");
      }
    } else {
      bufLink.updateFrom(link);

      if (link.isComplete() || bufLink.getBreedIndex() != sortedKey.breedIndex()) {
        sortedLinks.remove(sortedKey);
        sortedKey = new LinkKey(link.id(), link.end1Id(), link.end2Id(), link.getBreedIndex());
        sortedLinks.put(sortedKey, bufLink);
        linkKeys.put(simpleKey, sortedKey);
      }
    }
  }

  private void handleError(Object o) {
    if (printErrors) {
      System.err.println("@ " + new java.util.Date() + " : ");
      System.err.println(o.toString());
    }
  }

  // used by TestClientWorld only
  TurtleData getTurtleDataByWho(long who) {
    TurtleKey key = turtleKeys.get(Long.valueOf(who));
    if (key != null) {
      return sortedTurtles.get(key);
    }
    return null;
  }

  LinkData getLink(Long id) {
    LinkKey key = linkKeys.get(id);
    if (key == null) {
      LinkData link = uninitializedLinks.get(id);
      if (link == null) {
        link = new LinkData(id);
        uninitializedLinks.put(id, link);
      }
      return link;
    } else {
      return sortedLinks.get(key);
    }
  }

  TurtleData getTurtle(Long id) {
    TurtleKey key = turtleKeys.get(id);
    if (key == null) {
      TurtleData turtle = uninitializedTurtles.get(id);
      if (turtle == null) {
        turtle = new TurtleData(id);
        uninitializedTurtles.put(id, turtle);
      }
      return turtle;
    } else {
      return sortedTurtles.get(key);
    }
  }

  public enum PerspectiveMode {
    SERVER, CLIENT;
  }

  private PerspectiveMode perspectiveMode = PerspectiveMode.SERVER;

  public Perspective perspective = PerspectiveJ.OBSERVE();
  public AgentData targetAgent;
  public double radius;

  public boolean serverMode() {
    return perspectiveMode == PerspectiveMode.SERVER;
  }

  public void updateServerPerspective(AgentPerspective p) {
    if (perspectiveMode == PerspectiveMode.SERVER) {
      perspective = Perspective.load(p.perspective());
      radius = p.radius();
      targetAgent = getAgent(p.agent());
    }
  }

  public void updateClientPerspective(AgentPerspective p) {
    perspective = Perspective.load(p.perspective());
    perspectiveMode = p.serverMode() ? PerspectiveMode.SERVER : PerspectiveMode.CLIENT;
    targetAgent = getAgent(p.agent());
    radius = p.radius();
  }

  public double followOffsetX() {
    if (targetAgent == null || (perspective != PerspectiveJ.FOLLOW() && perspective != PerspectiveJ.RIDE())) {
      return 0;
    }

    if (perspectiveMode == PerspectiveMode.CLIENT) {
      return targetAgent.xcor() - ((viewWidth() - 1) / 2) - minPxcor();
    } else {
      return targetAgent.xcor() - ((minPxcor() - 0.5) + worldWidth() / 2.0);
    }
  }

  public double followOffsetY() {
    AgentData agent = targetAgent();

    if (agent == null || (perspective != PerspectiveJ.FOLLOW() && perspective != PerspectiveJ.RIDE())) {
      return 0;
    }

    if (perspectiveMode == PerspectiveMode.CLIENT) {
      return targetAgent.ycor() + ((viewHeight() - 1) / 2) - maxPycor();
    } else {
      return targetAgent.ycor() - ((minPycor() - 0.5) + worldHeight() / 2.0);
    }
  }

  public AgentData targetAgent() {
    return targetAgent;
  }

  private int viewWidth;
  private int viewHeight;

  public void viewWidth(int viewWidth) {
    this.viewWidth = viewWidth;
  }

  public void viewHeight(int viewHeight) {
    this.viewHeight = viewHeight;
  }

  public double viewWidth() {
    return (viewWidth / patchSize());
  }

  public double viewHeight() {
    return (viewHeight / patchSize());
  }

  AgentData getAgent(Agent agent) {
    if (agent.tyype() == AgentTypeJ.TURTLE()) {
      return getTurtle(Long.valueOf(agent.id()));
    }
    if (agent.tyype() == AgentTypeJ.PATCH()) {
      return patchData[(int) agent.id()];
    }
    if (agent.tyype() == AgentTypeJ.LINK()) {
      return getLink(Long.valueOf(agent.id()));
    }
    return null;
  }

}
