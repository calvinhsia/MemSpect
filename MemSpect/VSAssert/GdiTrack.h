//#include "pch.h"
#include <commctrl.h>

HDC __stdcall Mine_BeginPaint(HWND a0, LPPAINTSTRUCT a1);
HICON __stdcall Mine_CopyIcon(HICON a0);
HANDLE __stdcall Mine_CopyImage(HANDLE a0, UINT a1, int a2, int a3, UINT a4);
HMETAFILE __stdcall Mine_CopyMetaFileA(HMETAFILE a0, LPCSTR a1);
HMETAFILE __stdcall Mine_CopyMetaFileW(HMETAFILE a0, LPCWSTR a1);
HBITMAP __stdcall Mine_CreateBitmap(int a0, int a1, UINT a2, UINT a3, void* a4);
HBITMAP __stdcall Mine_CreateBitmapIndirect(BITMAP* a0);

HBRUSH __stdcall Mine_CreateBrushIndirect(struct tagLOGBRUSH* a0);
HCOLORSPACE __stdcall Mine_CreateColorSpaceA(struct tagLOGCOLORSPACEA* a0);
HCOLORSPACE __stdcall Mine_CreateColorSpaceW(struct tagLOGCOLORSPACEW* a0);
HBITMAP __stdcall Mine_CreateCompatibleBitmap(HDC a0, int a1, int a2);
HDC __stdcall Mine_CreateCompatibleDC(HDC a0);
HCURSOR __stdcall Mine_CreateCursor(HINSTANCE a0, int a1, int a2, int a3, int a4, void* a5, void* a6);

HDC __stdcall Mine_CreateDCA(LPCSTR a0, LPCSTR a1, LPCSTR a2, struct _devicemodeA* a3);
HDC __stdcall Mine_CreateDCW(LPCWSTR a0, LPCWSTR a1, LPCWSTR a2, struct _devicemodeW* a3);
HBRUSH __stdcall Mine_CreateDIBPatternBrush(HGLOBAL a0, UINT a1);
HBRUSH __stdcall Mine_CreateDIBPatternBrushPt(void* a0, UINT a1);
HBITMAP __stdcall Mine_CreateDIBSection(HDC a0, struct tagBITMAPINFO* a1, UINT a2, void** a3, HANDLE a4, DWORD a5);
HBITMAP __stdcall Mine_CreateDIBitmap(HDC a0, BITMAPINFOHEADER* a1, DWORD a2, void* a3, struct tagBITMAPINFO* a4, UINT a5);
/*
HBITMAP __stdcall Mine_CreateDiscardableBitmap(HDC a0,int a1,int a2);
*/
HRGN __stdcall Mine_CreateEllipticRgn(int a0, int a1, int a2, int a3);
HRGN __stdcall Mine_CreateEllipticRgnIndirect(RECT* a0);
HDC __stdcall Mine_CreateEnhMetaFileA(HDC a0, LPCSTR a1, RECT* a2, LPCSTR a3);
HDC __stdcall Mine_CreateEnhMetaFileW(HDC a0, LPCWSTR a1, RECT* a2, LPCWSTR a3);
HFONT __stdcall Mine_CreateFontA(int a0, int a1, int a2, int a3, int a4, DWORD a5, DWORD a6, DWORD a7, DWORD a8, DWORD a9, DWORD a10, DWORD a11, DWORD a12, LPCSTR a13);
HFONT __stdcall Mine_CreateFontW(int a0, int a1, int a2, int a3, int a4, DWORD a5, DWORD a6, DWORD a7, DWORD a8, DWORD a9, DWORD a10, DWORD a11, DWORD a12, LPCWSTR a13);
HFONT __stdcall Mine_CreateFontIndirectA(LOGFONTA* a0);
HFONT __stdcall Mine_CreateFontIndirectW(LOGFONTW* a0);

#if (_WIN32_WINNT >= 0x0500)
HFONT __stdcall Mine_CreateFontIndirectExA(ENUMLOGFONTEXDVA * a0);
HFONT __stdcall Mine_CreateFontIndirectExW(ENUMLOGFONTEXDVW * a0);
#endif //(_WIN32_WINNT >= 0x0500)

HPALETTE __stdcall Mine_CreateHalftonePalette(HDC a0);
HBRUSH __stdcall Mine_CreateHatchBrush(int a0, COLORREF a1);
HDC __stdcall Mine_CreateICA(LPCSTR a0, LPCSTR a1, LPCSTR a2, struct _devicemodeA* a3);
HDC __stdcall Mine_CreateICW(LPCWSTR a0, LPCWSTR a1, LPCWSTR a2, struct _devicemodeW* a3);
HICON __stdcall Mine_CreateIcon(HINSTANCE a0,int a1,int a2,BYTE a3,BYTE a4,BYTE* a5,BYTE* a6);
HICON __stdcall Mine_CreateIconFromResource(PBYTE a0,DWORD a1,BOOL a2,DWORD a3);
HICON __stdcall Mine_CreateIconFromResourceEx(PBYTE a0,DWORD a1,BOOL a2,DWORD a3,int a4,int a5,UINT a6);

HICON __stdcall Mine_CreateIconIndirect(struct _ICONINFO* a0);
HICON __stdcall Mine_DuplicateIcon(HINSTANCE a0, HICON a1);
HMENU __stdcall Mine_CreateMenu(void);
HDC __stdcall Mine_CreateMetaFileA(LPCSTR a0);
HDC __stdcall Mine_CreateMetaFileW(LPCWSTR a0);
HPALETTE __stdcall Mine_CreatePalette(LOGPALETTE* a0);

HBRUSH __stdcall Mine_CreatePatternBrush(HBITMAP a0);
HPEN __stdcall Mine_CreatePen(int a0, int a1, COLORREF a2);
HPEN __stdcall Mine_CreatePenIndirect(LOGPEN* a0);
HRGN __stdcall Mine_CreatePolyPolygonRgn(POINT* a0, INT* a1, int a2, int a3);
HRGN __stdcall Mine_CreatePolygonRgn(POINT* a0, int a1, int a2);
HMENU __stdcall Mine_CreatePopupMenu(void);
HRGN __stdcall Mine_CreateRectRgn(int a0, int a1, int a2, int a3);
HRGN __stdcall Mine_CreateRectRgnIndirect(RECT* a0);
HRGN __stdcall Mine_CreateRoundRectRgn(int a0, int a1, int a2, int a3, int a4, int a5);

HBRUSH __stdcall Mine_CreateSolidBrush(COLORREF a0);

BOOL __stdcall Mine_DeleteColorSpace(HCOLORSPACE a0);
BOOL __stdcall Mine_DeleteDC(HDC a0);
BOOL __stdcall Mine_DeleteEnhMetaFile(HENHMETAFILE a0);
BOOL __stdcall Mine_DeleteMetaFile(HMETAFILE a0);
BOOL __stdcall Mine_DeleteObject(HGDIOBJ a0);
BOOL __stdcall Mine_DestroyAcceleratorTable(HACCEL a0);
BOOL __stdcall Mine_DestroyCursor(HCURSOR a0);
BOOL __stdcall Mine_DestroyIcon(HICON a0);
BOOL __stdcall Mine_DestroyMenu(HMENU a0);

BOOL __stdcall Mine_EndPaint(HWND a0, PAINTSTRUCT* a1);

HPEN __stdcall Mine_ExtCreatePen(DWORD a0, DWORD a1, struct tagLOGBRUSH* a2, DWORD a3, DWORD* a4);
HRGN __stdcall Mine_ExtCreateRegion(XFORM* a0, DWORD a1, RGNDATA* a2);
HICON __stdcall Mine_ExtractAssociatedIconA(HINSTANCE a0, _In_z_ LPSTR a1, _Inout_ LPWORD a2);
HICON __stdcall Mine_ExtractAssociatedIconW(HINSTANCE a0, _In_z_ LPWSTR a1, _Inout_ LPWORD a2);
HICON __stdcall Mine_ExtractIconA(HINSTANCE a0, LPCSTR a1, UINT a2);
HICON __stdcall Mine_ExtractIconW(HINSTANCE a0, LPCWSTR a1, UINT a2);
UINT __stdcall Mine_ExtractIconExA(LPCSTR pszIconFile, int index, HICON* phLarge, HICON* phSmall, UINT uiIcon);
UINT __stdcall Mine_ExtractIconExW(LPCWSTR pszIconFile, int index, HICON* phLarge, HICON* phSmall, UINT uiIcon);
/*
BOOL __stdcall Mine_FreeLibrary(HMODULE a0);
void __stdcall Mine_FreeLibraryAndExitThread(HMODULE a0,DWORD a1);
*/
HDC __stdcall Mine_GetDC(HWND a0);
HDC __stdcall Mine_GetDCEx(HWND a0, HRGN a1, DWORD a2);
HENHMETAFILE __stdcall Mine_GetEnhMetaFileA(LPCSTR a0);
HENHMETAFILE __stdcall Mine_GetEnhMetaFileW(LPCWSTR a0);
HMETAFILE __stdcall Mine_GetMetaFileA(LPCSTR a0);
HMETAFILE __stdcall Mine_GetMetaFileW(LPCWSTR a0);
HDC __stdcall Mine_GetWindowDC(HWND a0);

HIMAGELIST __stdcall Mine_ImageList_Create(int a0, int a1, UINT a2, int a3, int a4);
BOOL __stdcall Mine_ImageList_Destroy(HIMAGELIST a0);
HIMAGELIST __stdcall Mine_ImageList_Duplicate(HIMAGELIST a0);
HIMAGELIST __stdcall Mine_ImageList_LoadImageA(HINSTANCE a0, LPCSTR a1, int a2, int a3, COLORREF a4, UINT a5, UINT a6);
HIMAGELIST __stdcall Mine_ImageList_LoadImageW(HINSTANCE a0, LPCWSTR a1, int a2, int a3, COLORREF a4, UINT a5, UINT a6);
HIMAGELIST __stdcall Mine_ImageList_Merge(HIMAGELIST a0, int a1, HIMAGELIST a2, int a3, int a4, int a5);
HIMAGELIST __stdcall Mine_ImageList_Read(LPSTREAM a0);

HACCEL __stdcall Mine_LoadAcceleratorsA(HINSTANCE a0, LPCSTR a1);
HACCEL __stdcall Mine_LoadAcceleratorsW(HINSTANCE a0, LPCWSTR a1);
HBITMAP __stdcall Mine_LoadBitmapA(HINSTANCE a0, LPCSTR a1);
HBITMAP __stdcall Mine_LoadBitmapW(HINSTANCE a0, LPCWSTR a1);
HCURSOR __stdcall Mine_LoadCursorA(HINSTANCE a0, LPCSTR a1);
HCURSOR __stdcall Mine_LoadCursorW(HINSTANCE a0, LPCWSTR a1);
HCURSOR __stdcall Mine_LoadCursorFromFileA(LPCSTR a0);
HCURSOR __stdcall Mine_LoadCursorFromFileW(LPCWSTR a0);
HICON __stdcall Mine_LoadIconA(HINSTANCE a0,LPCSTR a1);
HICON __stdcall Mine_LoadIconW(HINSTANCE a0,LPCWSTR a1);
HANDLE __stdcall Mine_LoadImageA(HINSTANCE a0, LPCSTR a1, UINT a2, int a3, int a4, UINT a5);
HANDLE __stdcall Mine_LoadImageW(HINSTANCE a0, LPCWSTR a1, UINT a2, int a3, int a4, UINT a5);
HMENU __stdcall Mine_LoadMenuA(HINSTANCE a0, LPCSTR a1);
HMENU __stdcall Mine_LoadMenuW(HINSTANCE a0, LPCWSTR a1);
HMENU __stdcall Mine_LoadMenuIndirectA(MENUTEMPLATEA* a0);
HMENU __stdcall Mine_LoadMenuIndirectW(MENUTEMPLATEW* a0);
//HGLOBAL __stdcall Mine_LoadResource(HMODULE a0, HRSRC a1);

int __stdcall Mine_ReleaseDC(HWND a0, HDC a1);
//int __stdcall Mine_SetWindowRgn(HWND a0, HRGN a1, BOOL a2);
HMETAFILE __stdcall Mine_SetMetaFileBitsEx(UINT a0, BYTE* a1);
HENHMETAFILE __stdcall Mine_SetWinMetaFileBits(UINT a0, BYTE* a1, HDC a2, struct tagMETAFILEPICT* a3);
DWORD_PTR __stdcall Mine_SHGetFileInfoA(LPCSTR a0, DWORD a1, SHFILEINFOA *a2, UINT a3, UINT a4);
DWORD_PTR __stdcall Mine_SHGetFileInfoW(LPCWSTR a0, DWORD a1, SHFILEINFOW *a2, UINT a3, UINT a4);

//declare real function pointers
decltype(&Mine_BeginPaint) Real_BeginPaint;
decltype(&Mine_CopyIcon) Real_CopyIcon;
decltype(&Mine_CopyImage) Real_CopyImage;
decltype(&Mine_CopyMetaFileA) Real_CopyMetaFileA;
decltype(&Mine_CopyMetaFileW) Real_CopyMetaFileW;
decltype(&Mine_CreateBitmap) Real_CreateBitmap;
decltype(&Mine_CreateBitmapIndirect) Real_CreateBitmapIndirect;
decltype(&Mine_CreateBrushIndirect) Real_CreateBrushIndirect;
decltype(&Mine_CreateColorSpaceA) Real_CreateColorSpaceA;
decltype(&Mine_CreateColorSpaceW) Real_CreateColorSpaceW;
decltype(&Mine_CreateCompatibleBitmap) Real_CreateCompatibleBitmap;
decltype(&Mine_CreateCompatibleDC) Real_CreateCompatibleDC;
decltype(&Mine_CreateCursor) Real_CreateCursor;
decltype(&Mine_CreateDCA) Real_CreateDCA;
decltype(&Mine_CreateDCW) Real_CreateDCW;
decltype(&Mine_CreateDIBPatternBrush) Real_CreateDIBPatternBrush;
decltype(&Mine_CreateDIBPatternBrushPt) Real_CreateDIBPatternBrushPt;
decltype(&Mine_CreateDIBSection) Real_CreateDIBSection;
decltype(&Mine_CreateDIBitmap) Real_CreateDIBitmap;
//decltype(&Mine_CreateDiscardableBitmap) Real_CreateDiscardableBitmap;
decltype(&Mine_CreateEllipticRgn) Real_CreateEllipticRgn;
decltype(&Mine_CreateEllipticRgnIndirect) Real_CreateEllipticRgnIndirect;
decltype(&Mine_CreateEnhMetaFileA) Real_CreateEnhMetaFileA;
decltype(&Mine_CreateEnhMetaFileW) Real_CreateEnhMetaFileW;
decltype(&Mine_CreateFontA) Real_CreateFontA;
decltype(&Mine_CreateFontW) Real_CreateFontW;
decltype(&Mine_CreateFontIndirectA) Real_CreateFontIndirectA;
decltype(&Mine_CreateFontIndirectW) Real_CreateFontIndirectW;

#if (_WIN32_WINNT >= 0x0500)
decltype(&Mine_CreateFontIndirectExA) Real_CreateFontIndirectExA;
decltype(&Mine_CreateFontIndirectExW) Real_CreateFontIndirectExW;
#endif //(_WIN32_WINNT >= 0x0500)

decltype(&Mine_CreateHalftonePalette) Real_CreateHalftonePalette;
decltype(&Mine_CreateHatchBrush) Real_CreateHatchBrush;
decltype(&Mine_CreateICA) Real_CreateICA;
decltype(&Mine_CreateICW) Real_CreateICW;
decltype(&Mine_CreateIcon) Real_CreateIcon;
decltype(&Mine_CreateIconFromResource) Real_CreateIconFromResource;
decltype(&Mine_CreateIconFromResourceEx) Real_CreateIconFromResourceEx;
decltype(&Mine_CreateIconIndirect) Real_CreateIconIndirect;
decltype(&Mine_CreateMenu) Real_CreateMenu;
decltype(&Mine_CreateMetaFileA) Real_CreateMetaFileA;
decltype(&Mine_CreateMetaFileW) Real_CreateMetaFileW;
decltype(&Mine_CreatePalette) Real_CreatePalette;
decltype(&Mine_CreatePatternBrush) Real_CreatePatternBrush;
decltype(&Mine_CreatePen) Real_CreatePen;
decltype(&Mine_CreatePenIndirect) Real_CreatePenIndirect;
decltype(&Mine_CreatePolyPolygonRgn) Real_CreatePolyPolygonRgn;
decltype(&Mine_CreatePolygonRgn) Real_CreatePolygonRgn;
decltype(&Mine_CreatePopupMenu) Real_CreatePopupMenu;
decltype(&Mine_CreateRectRgn) Real_CreateRectRgn;
decltype(&Mine_CreateRectRgnIndirect) Real_CreateRectRgnIndirect;
decltype(&Mine_CreateRoundRectRgn) Real_CreateRoundRectRgn;
decltype(&Mine_CreateSolidBrush) Real_CreateSolidBrush;
decltype(&Mine_DeleteColorSpace) Real_DeleteColorSpace;
decltype(&Mine_DeleteDC) Real_DeleteDC;
decltype(&Mine_DeleteEnhMetaFile) Real_DeleteEnhMetaFile;
decltype(&Mine_DeleteMetaFile) Real_DeleteMetaFile;
decltype(&Mine_DeleteObject) Real_DeleteObject;
decltype(&Mine_DestroyAcceleratorTable) Real_DestroyAcceleratorTable;
decltype(&Mine_DestroyCursor) Real_DestroyCursor;
decltype(&Mine_DestroyIcon) Real_DestroyIcon;
decltype(&Mine_DestroyMenu) Real_DestroyMenu;
decltype(&Mine_DuplicateIcon) Real_DuplicateIcon;
decltype(&Mine_EndPaint) Real_EndPaint;
decltype(&Mine_ExtCreatePen) Real_ExtCreatePen;
decltype(&Mine_ExtCreateRegion) Real_ExtCreateRegion;
decltype(&Mine_ExtractAssociatedIconA) Real_ExtractAssociatedIconA;
decltype(&Mine_ExtractAssociatedIconW) Real_ExtractAssociatedIconW;
decltype(&Mine_ExtractIconA) Real_ExtractIconA;
decltype(&Mine_ExtractIconW) Real_ExtractIconW;
decltype(&Mine_ExtractIconExA) Real_ExtractIconExA;
decltype(&Mine_ExtractIconExW) Real_ExtractIconExW;
//decltype(&Mine_FreeLibrary) Real_FreeLibrary;
//decltype(&Mine_FreeLibraryAndExitThread) Real_FreeLibraryAndExitThread;
decltype(&Mine_GetDC) Real_GetDC;
decltype(&Mine_GetDCEx) Real_GetDCEx;
decltype(&Mine_GetEnhMetaFileA) Real_GetEnhMetaFileA;
decltype(&Mine_GetEnhMetaFileW) Real_GetEnhMetaFileW;
decltype(&Mine_GetMetaFileA) Real_GetMetaFileA;
decltype(&Mine_GetMetaFileW) Real_GetMetaFileW;
decltype(&Mine_GetWindowDC) Real_GetWindowDC;
decltype(&Mine_ImageList_Create) Real_ImageList_Create;
decltype(&Mine_ImageList_Destroy) Real_ImageList_Destroy;
decltype(&Mine_ImageList_Duplicate) Real_ImageList_Duplicate;
decltype(&Mine_ImageList_LoadImageA) Real_ImageList_LoadImageA;
decltype(&Mine_ImageList_LoadImageW) Real_ImageList_LoadImageW;
decltype(&Mine_ImageList_Merge) Real_ImageList_Merge;
decltype(&Mine_ImageList_Read) Real_ImageList_Read;
decltype(&Mine_LoadAcceleratorsA) Real_LoadAcceleratorsA;
decltype(&Mine_LoadAcceleratorsW) Real_LoadAcceleratorsW;
decltype(&Mine_LoadBitmapA) Real_LoadBitmapA;
decltype(&Mine_LoadBitmapW) Real_LoadBitmapW;
decltype(&Mine_LoadCursorA) Real_LoadCursorA;
decltype(&Mine_LoadCursorW) Real_LoadCursorW;
decltype(&Mine_LoadCursorFromFileA) Real_LoadCursorFromFileA;
decltype(&Mine_LoadCursorFromFileW) Real_LoadCursorFromFileW;
decltype(&Mine_LoadIconA) Real_LoadIconA;
decltype(&Mine_LoadIconW) Real_LoadIconW;
decltype(&Mine_LoadImageA) Real_LoadImageA;
decltype(&Mine_LoadImageW) Real_LoadImageW;
decltype(&Mine_LoadMenuA) Real_LoadMenuA;
decltype(&Mine_LoadMenuW) Real_LoadMenuW;
decltype(&Mine_LoadMenuIndirectA) Real_LoadMenuIndirectA;
decltype(&Mine_LoadMenuIndirectW) Real_LoadMenuIndirectW;
//decltype(&Mine_LoadResource) Real_LoadResource;
decltype(&Mine_ReleaseDC) Real_ReleaseDC;
//decltype(&Mine_SetWindowRgn) Real_SetWindowRgn;
decltype(&Mine_SetMetaFileBitsEx) Real_SetMetaFileBitsEx;
decltype(&Mine_SetWinMetaFileBits) Real_SetWinMetaFileBits;
decltype(&Mine_SHGetFileInfoA) Real_SHGetFileInfoA;
decltype(&Mine_SHGetFileInfoW) Real_SHGetFileInfoW;
