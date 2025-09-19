# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Pert Canvas is a ClojureScript application for creating interactive PERT (Program Evaluation and Review Technique) diagrams from task lists with dependencies. The app visualizes critical paths in projects using ReactFlow for the diagram canvas and MUI DataGrid for task editing.

## Development Commands

- **Start development server**: `npx shadow-cljs watch frontend` or `./start.sh`
  - Serves on http://localhost:8080
  - NREPL on port 7002
- **Build for production**: `npx shadow-cljs release frontend`
- **Connect REPL**: `npx shadow-cljs cljs-repl frontend`

## Architecture

The application follows a ClojureScript + re-frame architecture:

### Core Structure
- **State management**: re-frame with undo/redo support via `day8.re-frame/undo`
- **UI framework**: UIX (React wrapper) for components
- **Data validation**: Malli schemas for task structure validation
- **Layout engine**: Dagre for automatic node positioning in flow diagrams

### Key Namespaces
- `pert-canvas.ui.app`: Main application component and initialization
- `pert-canvas.ui.state`: State schemas and initial data using Malli
- `pert-canvas.ui.events`: Re-frame event handlers (undoable actions for task management)
- `pert-canvas.ui.subs`: Re-frame subscriptions for reactive data flow
- `pert-canvas.utils`: Utility functions for CSV parsing, layout calculations
- `redmine.api`: API integration for Plan.io/Redmine project management

### Data Flow
1. Tasks stored in re-frame app-db with schema validation
2. DataGrid edits dispatch events to update task state
3. ReactFlow subscribes to computed nodes/edges from task data
4. Dagre automatically layouts nodes based on dependencies
5. CSV import supports mapping columns to task fields

### Key Features
- Interactive PERT diagram with node selection and edge creation
- Task editing via MUI DataGrid with real-time validation
- CSV import/export with column mapping modal
- Drag-and-drop CSV file support
- Undo/redo functionality for all task operations
- Keyboard shortcuts: Cmd/Ctrl+Z (undo), Cmd/Ctrl+Shift+Z (redo), Delete/Backspace (delete selected)

## File Organization
- `src/main/pert_canvas/`: Core application code
- `src/main/redmine/`: External API integration
- `public/`: Static assets and compiled JavaScript output
- `shadow-cljs.edn`: Build configuration with React interop setup