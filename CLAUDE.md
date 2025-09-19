# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Pert Canvas is a ClojureScript application for creating interactive PERT (Program Evaluation and Review Technique) diagrams from task lists with dependencies. The app visualizes critical paths in projects using ReactFlow for the diagram canvas and MUI DataGrid for task editing.

## Development Commands

### Frontend
- **Start development server**: `npx shadow-cljs watch frontend` or `./start.sh`
  - Serves on http://localhost:8080
  - NREPL on port 7002
- **Build for production**: `npx shadow-cljs release frontend`
- **Connect REPL**: `npx shadow-cljs cljs-repl frontend`

### Backend (Plan.io API Proxy)
- **Install dependencies**: `clj -P`
- **Start backend server**: `clj -M:run`
  - Serves on http://localhost:3000
  - Proxies `/api/*` requests to Plan.io with authentication
- **Backend configuration**: Credentials stored in `.secret` file (gitignored)

## Architecture

The application follows a full-stack Clojure/ClojureScript architecture:

### Full-Stack Structure
- **Frontend**: ClojureScript + re-frame + UIX (React wrapper)
- **Backend**: Clojure server with Ring + Compojure for Plan.io API proxy
- **State management**: re-frame with undo/redo support via `day8.re-frame/undo`
- **Data validation**: Malli schemas for task structure validation
- **Layout engine**: Dagre for automatic node positioning in flow diagrams
- **HTTP client**: cljs-http for frontend API calls

### Key Namespaces

#### Frontend (ClojureScript)
- `pert-canvas.ui.app`: Main application component and initialization
- `pert-canvas.ui.state`: State schemas and initial data using Malli
- `pert-canvas.ui.events`: Re-frame event handlers (undoable actions, Plan.io integration)
- `pert-canvas.ui.subs`: Re-frame subscriptions for reactive data flow
- `pert-canvas.ui.components.planio-url-input`: Plan.io URL input component
- `pert-canvas.utils`: Utility functions for CSV parsing, layout calculations
- `redmine.api`: Plan.io API client for ClojureScript

#### Backend (Clojure)
- `server`: Ring/Compojure server with Plan.io API proxy and CORS handling

### Data Flow
1. Tasks stored in re-frame app-db with Malli schema validation
2. DataGrid edits dispatch events to update task state
3. ReactFlow subscribes to computed nodes/edges from task data
4. Dagre automatically layouts nodes based on dependencies
5. CSV import supports mapping columns to task fields
6. Plan.io URLs processed through backend proxy for authentication
7. Plan.io issues converted to PERT tasks with dependency mapping

### Key Features

#### Core PERT Functionality
- Interactive PERT diagram with node selection and edge creation
- Task editing via MUI DataGrid with real-time validation
- Automatic critical path visualization using Dagre layout engine
- Undo/redo functionality for all task operations
- Keyboard shortcuts: Cmd/Ctrl+Z (undo), Cmd/Ctrl+Shift+Z (redo), Delete/Backspace (delete selected)

#### Data Import/Export
- CSV import/export with column mapping modal
- Drag-and-drop CSV file support
- **Plan.io Integration**: Direct import from Plan.io/Redmine projects
  - Smart URL parsing (converts any Plan.io URL to JSON API format)
  - Automatic dependency extraction from "blocks" relationships
  - Real-time project data import with one-click conversion to PERT tasks
  - Authentication handled securely through backend proxy

## File Organization
- `src/main/pert_canvas/`: Core frontend application code
- `src/main/redmine/`: Plan.io API client integration
- `src/backend/`: Clojure backend server code
- `public/`: Static assets and compiled JavaScript output
- `shadow-cljs.edn`: Frontend build configuration with React interop setup
- `deps.edn`: Backend dependencies and tooling configuration
- `.secret`: Plan.io API credentials (gitignored)

## Plan.io Integration Usage
1. Copy any Plan.io URL from your browser (issues list, project view, etc.)
2. Paste into the Plan.io URL input field in the app
3. Click "Fetch" to retrieve issues with dependencies
4. Click "Import to PERT" to convert issues to PERT tasks
5. View your project's critical path in the interactive diagram